# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
"""bd subprocess access, snapshot cache and derived indices.

The only data path is the `bd` CLI (never Dolt/SQL, never
`.beads/*.jsonl` — upstream documents that file as a passive export).
One bulk read serves the whole board; details are fetched lazily and
cached on the issue dict (a refresh rebuilds every dict, which
invalidates the cache per revision).
"""

import copy
import json
import os
import subprocess


class BdError(Exception):
    def __init__(self, message, returncode=None, stderr=""):
        super().__init__(message)
        self.returncode = returncode
        self.stderr = stderr


def run_bd(bd_bin, args, repo, timeout=600):
    """Run `bd` with cwd=<repo>, no BEADS_DIR (workspace discovery).

    On this repository BEADS_DIR is refused ("unsafe location") while
    plain discovery works; the scratch-directory case is the opposite,
    so BEADS_DIR is a fallback, armed only when discovery fails.
    """
    env = {**os.environ}
    env.pop("BEADS_DIR", None)
    for beads_dir in (None, str(repo / ".beads")):
        if beads_dir is not None:
            env["BEADS_DIR"] = beads_dir
        proc = subprocess.run(
            [bd_bin, *args],
            capture_output=True,
            text=True,
            cwd=str(repo),
            env=env,
            timeout=timeout,
        )
        if proc.returncode == 0:
            return proc
        failed_without_fallback = (
            beads_dir is None
            and "no beads database found" not in proc.stderr
            and "cannot resolve workspace" not in proc.stderr
        )
        if failed_without_fallback:
            break
    raise BdError(
        f"bd {' '.join(args[:3])} failed: {proc.stderr.strip()}",
        returncode=proc.returncode,
        stderr=proc.stderr,
    )


def run_bd_json(bd_bin, args, repo, timeout=600):
    proc = run_bd(bd_bin, args, repo, timeout)
    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError as e:
        raise BdError(f"bd {' '.join(args[:3])}: invalid JSON output: {e}") from e


class BdCache:
    """Snapshot of all issues plus derived indices, rebuilt per refresh."""

    def __init__(self, bd_bin, repo):
        self.bd_bin = bd_bin
        self.repo = repo
        self.revision = 0
        self.issues = []
        self.by_id = {}
        self.cluster_root = {}
        self.reverse_deps = {}
        self.blocked = set()
        self.label_counts = {}
        self.error_count = 0
        self._prev_state = {}  # id -> rendered-field tuple of last snapshot
        self._pending = None  # refetched-but-uncommitted snapshot (dict)
        self._pending_delta = None  # diff of the pending snapshot
        self._prev_derived = {}  # id -> (cluster_root, blocked) of last commit
        self.refresh()

    def refresh(self):
        """Refetch and swap in one step; returns `{upserted, removed}`.

        Used at startup and wherever a lock-held subprocess is fine; the
        watcher uses begin_refetch/refresh_unlocked/commit_refetch so bd
        never runs under the lock.
        """
        self.begin_refetch()
        delta = self.refresh_unlocked()
        if not delta.get("error"):
            self.commit_refetch()
        return delta

    def begin_refetch(self):
        """Mark the cache as refetching (single-flight helper)."""
        self._pending = None

    def refresh_unlocked(self):
        """Run bd list WITHOUT touching live state; caller commits after.

        On bd failure returns {error: True}; the live snapshot stays.
        """
        try:
            issues = run_bd_json(
                self.bd_bin, ["list", "--all", "--limit", "0", "--json"], self.repo
            )
        except BdError:
            self.error_count += 1
            return {"upserted": [], "removed": [], "error": True}
        delta = self._diff(issues)
        self._pending = issues
        self._pending_delta = delta
        return delta

    def commit_refetch(self):
        """Swap the pending snapshot in under the caller's lock."""
        issues = self._pending
        if issues is None:
            return
        self._pending = None
        self.issues = issues
        self.by_id = {i["id"]: i for i in issues}
        self.revision += 1
        self._derive()
        # a dependency edit re-derives blocked-ness/lane of issues it
        # never touched, so the delta is completed after derivation:
        # every issue whose derived state changed joins the upserts
        if self._pending_delta is not None:
            self._complete_delta(self._pending_delta)
            self._pending_delta = None

    def _complete_delta(self, delta):
        """After _derive(): add issues whose DERIVED state changed.

        The rendered-field diff sees direct edits only; a dependency
        change re-derives blocked-ness and lane membership of issues
        that never changed themselves, and those must reach the
        client too or its cards strand outside their lane with stale
        blocker chips."""
        upserted = {i["id"]: i for i in delta["upserted"]}
        for issue_id, root in self.cluster_root.items():
            derived = (root, issue_id in self.blocked)
            if self._prev_derived.get(issue_id) == derived:
                continue
            if issue_id not in upserted and issue_id in self.by_id:
                delta["upserted"].append(copy.deepcopy(self.by_id[issue_id]))
            self._prev_derived[issue_id] = derived
        for issue_id in list(self._prev_derived):
            if issue_id not in self.by_id:
                del self._prev_derived[issue_id]

    def _diff(self, issues):
        """Diff by `id -> (updated_at, status, priority, assignee, title,
        labels)` into {upserted, removed} — every field a card renders."""
        def state(i):
            return (
                i.get("updated_at"), i.get("status"), i.get("priority"),
                i.get("assignee"), i.get("title"),
                tuple(sorted(i.get("labels", []))),
            )
        new_state = {i["id"]: state(i) for i in issues}
        delta = {"upserted": [], "removed": []}
        for i in issues:
            if self._prev_state.get(i["id"]) != new_state[i["id"]]:
                delta["upserted"].append(copy.deepcopy(i))
        for issue_id in self._prev_state:
            if issue_id not in new_state:
                delta["removed"].append(issue_id)
        self._prev_state = new_state
        return delta

    def _derive(self):
        """Rebuild the derived indices: clusterRoot, blocked, reverseDeps.

        `blocked` = status open and at least one non-closed `blocks`
        in-edge. `labels` is sparse (absent, not null, on most issues),
        so every consumer uses `|| []`-style `.get("labels", [])`.
        """
        by_id = self.by_id

        # clusterRoot: strict parent chains first, then discovered-from
        # so beads found while working on a lane member join that lane
        def parent_of(issue):
            p = issue.get("parent")
            if isinstance(p, str):
                return p
            if isinstance(p, dict) and p.get("id"):
                return p["id"]
            for d in issue.get("dependencies", []):
                if d.get("type") == "parent-child":
                    return d.get("depends_on_id")
            return None

        strict_root = {}
        for issue in self.issues:
            seen = {issue["id"]}
            cur = issue
            while True:
                p = parent_of(cur)
                if not p or p in seen or p not in by_id:
                    break
                seen.add(p)
                cur = by_id[p]
            strict_root[issue["id"]] = cur["id"]

        cluster_size = {}
        for root in strict_root.values():
            cluster_size[root] = cluster_size.get(root, 0) + 1
        def is_lane(root):
            return cluster_size.get(root, 0) > 1

        self.cluster_root = {}
        for issue in self.issues:
            root = strict_root[issue["id"]]
            if not is_lane(root):
                for d in issue.get("dependencies", []):
                    if d.get("type") != "discovered-from":
                        continue
                    target = strict_root.get(d.get("depends_on_id"))
                    if target is not None and is_lane(target):
                        root = target
                        break
            self.cluster_root[issue["id"]] = root
            issue["cluster_root"] = root

        # reverseDeps: id -> issues that depend on it
        self.reverse_deps = {}
        for issue in self.issues:
            for d in issue.get("dependencies", []):
                target = d.get("depends_on_id")
                if target in by_id:
                    self.reverse_deps.setdefault(target, []).append(
                        {"issue_id": issue["id"], "type": d.get("type")}
                    )

        self.blocked = set()
        for target, deps in self.reverse_deps.items():
            if by_id.get(target, {}).get("status") != "open":
                continue
            for d in deps:
                if d["type"] == "blocks" and by_id[d["issue_id"]]["status"] != "closed":
                    self.blocked.add(target)
                    break
        for issue_id in self.blocked:
            self.by_id[issue_id]["is_blocked"] = True

        # label counts for the UI badges
        self.label_counts = {}
        for issue in self.issues:
            for label in issue.get("labels", []):
                self.label_counts[label] = self.label_counts.get(label, 0) + 1

    def indices_payload(self):
        """Serializable derived-index view, built under the caller's lock."""
        return {
            "revision": self.revision,
            "reverse_deps": self.reverse_deps,
            "blockers": {
                k: sum(
                    1
                    for d in deps
                    if d["type"] == "blocks"
                    and self.by_id[d["issue_id"]]["status"] != "closed"
                )
                for k, deps in self.reverse_deps.items()
            },
            "label_counts": self.label_counts,
        }

    def enrich_delta(self, delta):
        """Attach derived fields and matching indices to a delta so a
        patch alone is enough to keep the client's view consistent
        (raw records would strand cards outside their lane and stale
        blocker chips). Caller holds the lock and has already committed."""
        for issue in delta.get("upserted", []):
            issue["cluster_root"] = self.cluster_root.get(issue["id"], issue["id"])
            issue["is_blocked"] = issue["id"] in self.blocked
        delta["indices"] = self.indices_payload()
        return delta

    def issue(self, issue_id, with_comments=False):
        """Full `bd show` record, lazily fetched and cached on the dict.

        `bd list` omits fields (description, notes, dependencies on some
        versions); `bd show` is the authoritative detail record. The
        cache lives on the issue dict itself, so a refresh — which
        rebuilds every dict — invalidates it per revision.
        """
        issue = self.by_id.get(issue_id)
        if issue is None:
            return None
        if with_comments and issue.get("comment_count", 0) > 0 and "comments" not in issue:
            shown = run_bd_json(
                self.bd_bin,
                ["show", issue_id, "--json", "--include-comments"],
                self.repo,
            )
            if isinstance(shown, list) and shown:
                issue["comments"] = shown[0].get("comments", [])
        return issue

    def fingerprint_paths(self):
        """(manifest, last-touched) paths whose mtime/size change on writes."""
        beads = self.repo / ".beads"
        candidates = []
        # .beads/embeddeddolt/<db>/.dolt/noms/manifest — db dir name varies
        emb = beads / "embeddeddolt"
        if emb.is_dir():
            for db_dir in sorted(emb.iterdir()):
                if db_dir.is_dir():
                    candidates.append(db_dir / ".dolt" / "noms" / "manifest")
        candidates.append(beads / "last-touched")
        return candidates
