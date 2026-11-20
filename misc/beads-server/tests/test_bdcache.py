# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
"""Unit tests for bdcache against the fake-bd shim — no Dolt, no network."""

import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))

from bdcache import BdCache, BdError  # noqa: E402

ISSUES = [
    {
        "id": "t-1",
        "title": "One",
        "status": "open",
        "priority": 2,
        "comment_count": 0,
        # no labels key at all — the sparse-record trap: real bd omits
        # the key entirely rather than emitting an empty list
        # bd dep add t-1 t-2: t-1 is blocked BY t-2 (the edge rides on
        # the blocked issue)
        "dependencies": [{"depends_on_id": "t-2", "type": "blocks"}],
    },
    {
        "id": "t-2",
        "title": "Two",
        "status": "closed",
        "priority": 1,
        "comment_count": 1,
        "labels": ["mysbx"],
    },
]


class FakeBdCase(unittest.TestCase):
    def setUp(self):
        self.repo = Path(tempfile.mkdtemp(prefix="bdcache-test-"))
        (self.repo / ".beads").mkdir()
        (self.repo / ".beads" / "issues.json").write_text(json.dumps(ISSUES))
        self.bd = str(HERE / "fake-bd.sh")

    def tearDown(self):
        shutil.rmtree(self.repo)

    def write_issues(self, issues):
        (self.repo / ".beads" / "issues.json").write_text(json.dumps(issues))


class TestSnapshot(FakeBdCase):
    def test_list_parses_and_indexes(self):
        cache = BdCache(self.bd, self.repo)
        self.assertEqual(len(cache.issues), 2)
        self.assertEqual(cache.by_id["t-2"]["title"], "Two")

    def test_missing_labels_key_is_handled(self):
        cache = BdCache(self.bd, self.repo)
        # t-1 has NO labels key: consumers must never see a KeyError
        self.assertEqual(cache.label_counts, {"mysbx": 1})
        for issue in cache.issues:
            issue.get("labels", [])  # must not raise

    def test_reverse_deps_derived(self):
        cache = BdCache(self.bd, self.repo)
        self.assertEqual(
            cache.reverse_deps["t-2"],
            [{"issue_id": "t-1", "type": "blocks"}],
        )

    def test_blocked_dep_semantics(self):
        # bd dep add A B means A is blocked BY B; the edge rides on the
        # blocked issue, so the fixture's t-1 (blocks -> t-2) means
        # t-1 is blocked by t-2, never the other way round
        issues = json.loads(json.dumps(ISSUES))
        issues[1]["status"] = "open"  # prerequisite unresolved
        self.write_issues(issues)
        cache = BdCache(self.bd, self.repo)
        self.assertEqual(cache.blocked, {"t-1"})
        self.assertTrue(cache.by_id["t-1"]["is_blocked"])
        self.assertEqual(cache.by_id["t-1"]["blocked_by_count"], 1)
        self.assertNotIn("is_blocked", cache.by_id["t-2"])

    def test_blocked_cleared_when_prerequisite_closes(self):
        # the fixture's prerequisite t-2 is already closed → t-1's
        # only blocker is resolved, so t-1 is NOT blocked
        cache = BdCache(self.bd, self.repo)
        self.assertEqual(cache.blocked, set())

    def test_blocked_when_prerequisite_open(self):
        issues = json.loads(json.dumps(ISSUES))
        issues[1]["status"] = "open"  # t-2, the prerequisite, reopens
        self.write_issues(issues)
        cache = BdCache(self.bd, self.repo)
        self.assertEqual(cache.blocked, {"t-1"})

    def test_blocked_clears_when_carrier_closes(self):
        issues = json.loads(json.dumps(ISSUES))
        issues[1]["status"] = "open"  # prerequisite open
        issues[0]["status"] = "closed"  # carrier closed → not shown blocked
        self.write_issues(issues)
        cache = BdCache(self.bd, self.repo)
        self.assertEqual(cache.blocked, set())

    def test_indices_payload_counts_blockers(self):
        issues = json.loads(json.dumps(ISSUES))
        issues[1]["status"] = "open"  # make the prerequisite unresolved
        self.write_issues(issues)
        cache = BdCache(self.bd, self.repo)
        payload = cache.indices_payload()
        self.assertEqual(payload["revision"], 1)
        # blockers[X] = unresolved beads X blocks: t-2 blocks t-1
        self.assertEqual(payload["blockers"]["t-2"], 1)
        # blocked_by[X] = unresolved prerequisites of X
        self.assertEqual(payload["blocked_by"], {"t-1": 1})

    def test_enrich_delta_carries_derived_fields(self):
        cache = BdCache(self.bd, self.repo)
        issues = json.loads(json.dumps(ISSUES))
        # reopening the prerequisite BLOCKS t-1 — a DERIVED change on
        # an issue whose rendered fields never changed
        issues[1]["status"] = "open"
        issues[1]["updated_at"] = "2026-01-02T00:00:00Z"
        self.write_issues(issues)
        delta = cache.refresh()
        cache.enrich_delta(delta)
        upserted = {i["id"]: i for i in delta["upserted"]}
        self.assertIn("t-1", upserted)  # blocked-ness of t-1 changed
        self.assertTrue(upserted["t-1"]["is_blocked"])
        self.assertEqual(upserted["t-1"]["cluster_root"], "t-1")
        self.assertEqual(delta["indices"]["revision"], cache.revision)
        self.assertEqual(delta["indices"]["blocked_by"], {"t-1": 1})


class TestDelta(FakeBdCase):
    def test_delta_upsert_and_remove(self):
        cache = BdCache(self.bd, self.repo)
        delta = cache.refresh()
        # second refresh with identical data: no changes
        self.assertEqual(delta["upserted"], [])
        self.assertEqual(delta["removed"], [])

        # change a status, drop an issue, add one
        issues = json.loads(json.dumps(ISSUES))
        issues[0]["status"] = "in_progress"
        issues[1]["status"] = "open"
        issues.append({"id": "t-3", "title": "Three", "status": "open",
                       "priority": 2, "comment_count": 0})
        self.write_issues(issues)
        delta = cache.refresh()
        upserted_ids = {i["id"] for i in delta["upserted"]}
        # t-1's and t-2's rendered fields changed; t-3 is new
        self.assertEqual(upserted_ids, {"t-1", "t-2", "t-3"})
        self.assertEqual(delta["removed"], [])
        self.assertEqual(cache.revision, 3)

    def test_removed_on_delete(self):
        cache = BdCache(self.bd, self.repo)
        self.write_issues([ISSUES[0]])
        delta = cache.refresh()
        self.assertEqual(delta["removed"], ["t-2"])

    def test_revision_monotonic(self):
        cache = BdCache(self.bd, self.repo)
        revs = [cache.revision]
        for _ in range(3):
            cache.refresh()
            revs.append(cache.revision)
        self.assertEqual(revs, [1, 2, 3, 4])


class TestBdError(FakeBdCase):
    def test_bd_failure_keeps_snapshot(self):
        cache = BdCache(self.bd, self.repo)
        (self.repo / ".beads" / "issues.json").unlink()
        before = len(cache.issues)
        delta = cache.refresh()
        self.assertTrue(delta.get("error"))
        self.assertEqual(cache.error_count, 1)
        self.assertEqual(len(cache.issues), before)
        self.assertEqual(cache.revision, 1)  # failed refetch: no bump

    def test_unknown_command_raises(self):
        from bdcache import run_bd_json

        with self.assertRaises(BdError):
            run_bd_json(self.bd, ["frobnicate"], self.repo)


class TestBeadsDirFallback(FakeBdCase):
    def test_discovery_preferred_and_fallback_path_taken(self):
        # run_bd prefers workspace discovery (cwd=<repo>, no BEADS_DIR)
        # and retries with BEADS_DIR=<repo>/.beads on discovery failure.
        # A bd binary that fails WITHOUT BEADS_DIR but succeeds with it
        # exercises the retry branch end to end.
        from bdcache import run_bd_json

        wrapper = self.repo / "picky-bd"
        wrapper.write_text(
            f"#!{shutil.which('bash') or '/usr/bin/env bash'}\n"
            "if [ -z \"${BEADS_DIR:-}\" ]; then\n"
            "  echo 'no beads database found' >&2\n"
            "  exit 1\n"
            "fi\n"
            f"exec '{self.bd}' \"$@\"\n"
        )
        wrapper.chmod(0o755)

        out = run_bd_json(str(wrapper), ["count", "--json"], self.repo)
        self.assertEqual(out["count"], 2)

    def test_hard_failure_not_masked_by_fallback(self):
        # a bd that fails for a real reason (not discovery) must raise,
        # not fall back
        from bdcache import BdError, run_bd_json

        wrapper = self.repo / "broken-bd"
        wrapper.write_text(
            f"#!{shutil.which('bash') or '/usr/bin/env bash'}\n"
            "echo 'permission denied' >&2\n"
            "exit 1\n"
        )
        wrapper.chmod(0o755)

        with self.assertRaises(BdError):
            run_bd_json(str(wrapper), ["count", "--json"], self.repo)


if __name__ == "__main__":
    unittest.main()
