# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
"""HTTP-level tests: API endpoints, SSE framing, 409 on guard loss,
read-only refusal — against the fake-bd shim on an ephemeral port."""

import json
import re
import shutil
import socket
import sys
import tempfile
import threading
import time
import unittest
import urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))

import server  # noqa: E402

ISSUES = [
    {"id": "t-1", "title": "One", "status": "open", "priority": 2,
     "comment_count": 0},
    {"id": "t-2", "title": "Two", "status": "closed", "priority": 1,
     "comment_count": 0},
]


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


class ServerCase(unittest.TestCase):
    """Boots the real handler stack against fake-bd, one server per case."""

    def setUp(self):
        self.repo = Path(tempfile.mkdtemp(prefix="bds-http-"))
        (self.repo / ".beads").mkdir()
        (self.repo / ".beads" / "issues.json").write_text(json.dumps(ISSUES))
        self.bd = str(HERE / "fake-bd.sh")
        self.port = free_port()
        self.server = None

    def tearDown(self):
        if self.server:
            threading.Thread(target=self.server.shutdown, daemon=True).start()
        shutil.rmtree(self.repo)

    def boot(self, allow_write=False):
        watcher = server.Watcher(self.bd, self.repo, poll_interval=60)
        handler = server.make_handler(watcher, allow_write)
        srv = server.ThreadingHTTPServer(
            ("127.0.0.1", self.port),
            lambda *a, **kw: handler(*a, directory=str(HERE.parent / "static"), **kw),
        )
        srv.daemon_threads = True
        threading.Thread(target=srv.serve_forever, daemon=True).start()
        self.server = srv
        self.watcher = watcher
        self.base = f"http://127.0.0.1:{self.port}"
        return watcher

    def read_sse(self, path, seconds=3.0, until=None):
        """Read raw bytes from an SSE endpoint until `until` appears
        (a frame marker like "event: patch") or the deadline passes."""
        sock = socket.create_connection(("127.0.0.1", self.port), timeout=seconds)
        sock.sendall(f"GET {path} HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n".encode())
        sock.settimeout(0.5)
        data = b""
        deadline = time.time() + seconds
        while time.time() < deadline:
            if until and b"\n\n" in data.partition(until.encode())[2]:
                break
            try:
                chunk = sock.recv(4096)
            except socket.timeout:
                continue
            if not chunk:
                break
            data += chunk
        sock.close()
        return data.decode(errors="replace")


    def get(self, path):
        with urllib.request.urlopen(self.base + path, timeout=10) as r:
            return r.status, json.loads(r.read())

    def post(self, path, body, headers=None):
        req = urllib.request.Request(
            self.base + path,
            data=json.dumps(body).encode(),
            headers={"Content-Type": "application/json", **(headers or {})},
            method="POST",
        )
        try:
            with urllib.request.urlopen(req, timeout=10) as r:
                return r.status, json.loads(r.read())
        except urllib.error.HTTPError as e:
            body = e.read() or b"{}"
            try:
                return e.code, json.loads(body)
            except json.JSONDecodeError:
                return e.code, {}  # send_error() bodies are HTML


class TestReadOnlyApi(ServerCase):
    def setUp(self):
        super().setUp()
        self.boot(allow_write=False)

    def test_api_beads(self):
        status, data = self.get("/api/beads")
        self.assertEqual(status, 200)
        self.assertEqual(len(data["issues"]), 2)
        self.assertEqual(data["revision"], 1)
        _, indices = self.get("/api/indices")
        self.assertEqual({key: data[key] for key in indices}, indices)

    def test_api_issue_detail(self):
        status, data = self.get("/api/beads/issue/t-1")
        self.assertEqual(status, 200)
        self.assertEqual(data["issue"]["title"], "One")

    def test_api_health(self):
        status, data = self.get("/api/health")
        self.assertEqual(status, 200)
        self.assertEqual(data["issues"], 2)

    def test_api_indices(self):
        status, data = self.get("/api/indices")
        self.assertEqual(status, 200)
        self.assertIn("label_counts", data)

    def test_capabilities_advertises_off(self):
        status, data = self.get("/api/capabilities")
        self.assertEqual(data["write"], False)

    def test_writes_refused_without_allow_write(self):
        status, _ = self.post(
            "/api/beads/issue/t-1/update", {"status": "closed"}
        )
        self.assertEqual(status, 403)

    def test_session_refused_without_allow_write(self):
        try:
            status, _ = self.get("/api/session")
        except urllib.error.HTTPError as e:
            self.assertEqual(e.code, 403)
        else:
            self.fail("expected 403")

    def test_reads_refuse_rebound_host(self):
        # DNS rebinding: attacker.com resolves to 127.0.0.1; the Host
        # header betrays it on reads too, not just on writes
        req = urllib.request.Request(
            self.base + "/api/beads", headers={"Host": "attacker.com"}
        )
        with self.assertRaises(urllib.error.HTTPError) as ctx:
            urllib.request.urlopen(req, timeout=10)
        self.assertEqual(ctx.exception.code, 403)

    def test_session_read_refuse_rebound_host(self):
        # the session token must never leak to a rebound origin
        req = urllib.request.Request(
            self.base + "/api/session", headers={"Host": "attacker.com"}
        )
        with self.assertRaises(urllib.error.HTTPError) as ctx:
            urllib.request.urlopen(req, timeout=10)
        self.assertEqual(ctx.exception.code, 403)


class TestWriteApi(ServerCase):
    def setUp(self):
        super().setUp()
        watcher = self.boot(allow_write=True)
        _, data = self.get("/api/session")
        self.token = data["token"]

    def test_update_roundtrip(self):
        status, data = self.post(
            "/api/beads/issue/t-1/update",
            {"status": "in_progress"},
            headers={"X-Beads-Server-Token": self.token},
        )
        self.assertEqual(status, 200)
        self.assertEqual(data["issue"]["status"], "in_progress")

    def test_guard_mismatch_is_409_with_fresh_issue(self):
        status, data = self.post(
            "/api/beads/issue/t-1/update",
            {"status": "in_progress", "if_status": "closed"},
            headers={"X-Beads-Server-Token": self.token},
        )
        self.assertEqual(status, 409)
        self.assertEqual(data["issue"]["status"], "open")
        # a lost guard must leave the DB untouched
        _, fresh = self.get("/api/beads/issue/t-1")
        self.assertEqual(fresh["issue"]["status"], "open")

    def test_409_carries_data_changed_by_external_writer(self):
        # external writer changes the issue behind the server's back;
        # the guard loss must surface THAT value, not the stale cache
        issues = json.loads(json.dumps(ISSUES))
        issues[0]["status"] = "in_progress"
        issues[0]["updated_at"] = "2026-01-02T00:00:00Z"
        (self.repo / ".beads" / "issues.json").write_text(json.dumps(issues))
        status, data = self.post(
            "/api/beads/issue/t-1/update",
            {"status": "closed", "if_status": "open"},
            headers={"X-Beads-Server-Token": self.token},
        )
        self.assertEqual(status, 409)
        self.assertEqual(data["issue"]["status"], "in_progress")

    def test_409_fresh_even_with_refetch_in_flight(self):
        # NOTE: superseded by test_overlapping_refetches_are_consistent,
        # kept for the direct single-flight regression
        # a background probe refetch must not starve the 409 of fresh
        # data: the conflict path forces its own refetch
        issues = json.loads(json.dumps(ISSUES))
        issues[0]["status"] = "in_progress"
        issues[0]["updated_at"] = "2026-01-02T00:00:00Z"
        (self.repo / ".beads" / "issues.json").write_text(json.dumps(issues))
        self.watcher._refetching = True  # simulate a probe mid-flight
        try:
            status, data = self.post(
                "/api/beads/issue/t-1/update",
                {"status": "closed", "if_status": "open"},
                headers={"X-Beads-Server-Token": self.token},
            )
        finally:
            self.watcher._refetching = False
        self.assertEqual(status, 409)
        self.assertEqual(data["issue"]["status"], "in_progress")

    def test_missing_token_is_403(self):
        status, _ = self.post("/api/beads/issue/t-1/update", {"status": "closed"})
        self.assertEqual(status, 403)

    def test_evil_origin_refused(self):
        status, _ = self.post(
            "/api/beads/issue/t-1/update",
            {"status": "closed"},
            headers={
                "X-Beads-Server-Token": self.token,
                "Origin": "https://localhost.evil.com",
            },
        )
        self.assertEqual(status, 403)

    def test_evil_host_refused(self):
        status, _ = self.post(
            "/api/beads/issue/t-1/update",
            {"status": "closed"},
            headers={"X-Beads-Server-Token": self.token, "Host": "evil.com"},
        )
        self.assertEqual(status, 403)

    def test_dual_stdin_fields_rejected(self):
        status, _ = self.post(
            "/api/beads/issue/t-1/update",
            {"description": "a", "design": "b"},
            headers={"X-Beads-Server-Token": self.token},
        )
        self.assertEqual(status, 400)

    def test_comment_requires_text(self):
        status, _ = self.post(
            "/api/beads/issue/t-1/comment",
            {"text": "  "},
            headers={"X-Beads-Server-Token": self.token},
        )
        self.assertEqual(status, 400)


class TestRefetchConsistency(ServerCase):
    def test_overlapping_refetches_are_consistent(self):
        # two concurrent refetch cycles share the cache's pending
        # slots; serialization must keep snapshot/delta/revision in
        # lockstep so no delta is published under the wrong revision
        self.boot(allow_write=False)

        results = []

        def run_refetch():
            results.append(self.watcher._refetch(force=True))

        t1 = threading.Thread(target=run_refetch)
        t1.start()
        # second forced refetch while the first is mid-cycle
        results.append(self.watcher._refetch(force=True))
        t1.join(timeout=10)

        # both cycles committed consistently: the SSE-visible revision
        # matches what a client subscribing now would get
        _, health = self.get("/api/health")
        _, beads = self.get("/api/beads")
        self.assertEqual(health["revision"], beads["revision"])
        self.assertEqual(len(beads["issues"]), len(ISSUES))

    def test_delta_never_published_under_wrong_revision(self):
        # a client subscribed before the overlap must see a patch whose
        # revision equals the snapshot the cache actually committed
        self.boot(allow_write=False)
        result = {}

        def reader():
            result["raw"] = self.read_sse(
                "/api/events?rev=1", seconds=8.0, until="event: patch"
            )

        t = threading.Thread(target=reader)
        t.start()
        deadline = time.time() + 8
        while time.time() < deadline:
            with self.watcher.lock:
                if self.watcher.listeners:
                    break
            time.sleep(0.05)
        self.assertTrue(self.watcher.listeners, "SSE client never subscribed")

        # two overlapping cycles + a data change: the patch the client
        # receives must reference the exact revisions the cache moved
        # through, never a delta from one cycle under the other's rev
        issues = json.loads(json.dumps(ISSUES))
        issues[0]["status"] = "in_progress"
        issues[0]["updated_at"] = "2026-01-02T00:00:00Z"
        (self.repo / ".beads" / "issues.json").write_text(json.dumps(issues))

        def run_refetch():
            self.watcher._refetch(force=True)

        t1 = threading.Thread(target=run_refetch)
        t1.start()
        self.watcher._refetch(force=True)
        t1.join(timeout=10)
        t.join(timeout=10)

        raw = result.get("raw", "")
        self.assertIn("event: patch", raw)
        # every patch event carries the revision it truly belongs to
        for m in re.finditer(r"event: patch\ndata: (.*)\n\n", raw):
            payload = json.loads(m.group(1))
            self.assertEqual(
                payload["revision"], payload["indices"]["revision"]
            )


class TestSse(ServerCase):
    def setUp(self):
        super().setUp()
        self.boot(allow_write=False)

    def test_sse_framing_hello_and_retry(self):
        raw = self.read_sse("/api/events?rev=0", until="event: hello")
        self.assertIn("retry: 2000", raw)
        self.assertIn("event: hello", raw)
        self.assertIn('"revision": 1', raw)

    def test_sse_reset_on_divergent_rev(self):
        # client ahead of a server restart → reset, not hello
        raw = self.read_sse("/api/events?rev=99", until="event: reset")
        self.assertIn("event: reset", raw)

    def test_snapshot_and_events_carry_generation(self):
        _, data = self.get("/api/beads")
        self.assertTrue(data.get("generation"))
        _, idx = self.get("/api/indices")
        self.assertEqual(idx["generation"], data["generation"])
        raw = self.read_sse("/api/events?rev=1", until="event: hello")
        self.assertIn('"generation"', raw)

    def test_sse_reset_on_foreign_generation(self):
        # a restart renumbers revisions; the same number means a
        # different state — a client presenting the OLD generation
        # must get a reset even at a matching revision
        _, data = self.get("/api/beads")
        raw = self.read_sse(
            f"/api/events?rev={data['revision']}&gen=old-process",
            until="event: reset",
        )
        self.assertIn("event: reset", raw)
        self.assertIn('"generation"', raw)

    def test_restart_resets_generation_and_client_recovers(self):
        # simulate a restart: a NEW watcher instance has a different
        # generation and starts revisions at 1 again
        _, before = self.get("/api/beads")
        old_gen = before["generation"]
        raw = self.read_sse(
            f"/api/events?rev=1&gen={old_gen}", until="event: hello"
        )
        self.assertIn("event: hello", raw)

        watcher2 = server.Watcher(self.bd, self.repo, poll_interval=60)
        self.assertNotEqual(watcher2.generation, old_gen)
        self.assertEqual(watcher2.cache.revision, 1)

    def test_sse_patch_on_change(self):
        # subscribe at rev=1 and wait for the server to register the
        # client, THEN change the data and refetch: the connected
        # client must receive a patch, not a reset
        result = {}

        def reader():
            result["raw"] = self.read_sse(
                "/api/events?rev=1", seconds=10.0, until="event: patch"
            )

        t = threading.Thread(target=reader)
        t.start()
        # wait until the watcher sees the subscribed listener
        deadline = time.time() + 5
        while time.time() < deadline:
            with self.watcher.lock:
                if self.watcher.listeners:
                    break
            time.sleep(0.05)
        self.assertTrue(self.watcher.listeners, "SSE client never subscribed")

        issues = json.loads(json.dumps(ISSUES))
        issues[0]["status"] = "in_progress"
        issues[0]["updated_at"] = "2026-01-02T00:00:00Z"
        (self.repo / ".beads" / "issues.json").write_text(json.dumps(issues))
        self.watcher._refetch()
        t.join(timeout=10)
        raw = result.get("raw", "")
        self.assertIn("event: patch", raw)
        patch = json.loads(raw.split("event: patch\ndata: ", 1)[1].split("\n\n", 1)[0])
        self.assertEqual(patch["indices"]["generation"], patch["generation"])
        self.assertEqual(patch["indices"]["revision"], patch["revision"])
        # the upserted record must carry the derived fields, or the
        # client's card strands outside its lane with a stale chip
        self.assertIn("cluster_root", raw)
        self.assertIn("is_blocked", raw)


if __name__ == "__main__":
    unittest.main()
