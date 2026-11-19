#!/usr/bin/env python3
"""beads-server: kanban board over the bd (beads) issue DB.

Serves the static frontend plus a JSON API over a cached `bd list
--all` snapshot: board, graph and details pane, `/api/health`,
`/api/capabilities`, and Server-Sent Events at `/api/events` for live
updates. A watcher thread probes the Dolt files (stat fingerprint,
`bd count` fallback) and refetches the snapshot when they change.

Usage:
    server.py [--host HOST] [--port PORT] [--repo PATH]
              [--poll-interval SEC]
"""

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

import argparse
import json
import os
import queue
import re
import subprocess
import sys
import threading
import time
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, unquote, urlparse

sys.path.insert(0, str(Path(__file__).resolve().parent))
from bdcache import BdCache, BdError, run_bd, run_bd_json  # noqa: E402

HERE = Path(__file__).resolve().parent / "static"

PROBE_TICKS = 15          # every Nth tick: authoritative bd count probe
SSE_HEARTBEAT = 15.0      # seconds between SSE heartbeat comments



class Watcher:
    """Owns the snapshot cache; probes cheap, refetches single-flight."""

    def __init__(self, bd_bin, repo, poll_interval):
        self.bd_bin = bd_bin
        self.repo = repo
        self.poll_interval = poll_interval
        self.cache = BdCache(bd_bin, repo)
        self.lock = threading.Lock()
        self._fingerprints = {}
        self._tick = 0
        self._refetching = False  # single-flight guard
        self.bd_mode = self._probe_mode()
        self.listeners = []  # {"q": Queue, "rev": int} per SSE client
        self.capture_fingerprint()

    def _probe_mode(self):
        try:
            run_bd(self.bd_bin, ["count", "--json"], self.repo)
            return "workspace-discovery"
        except BdError:
            return "beads-dir-fallback"

    def snapshot(self):
        """Consistent (revision, issues) pair, safe against probe swaps."""
        with self.lock:
            return self.cache.revision, self.cache.issues

    def fingerprint_changed(self):
        try:
            for p in self.cache.fingerprint_paths():
                st = p.stat()
                key = (st.st_mtime_ns, st.st_size)
                if key != self._fingerprints.get(str(p)):
                    return True
            return False
        except OSError:
            return True  # paths missing (e.g. fresh scratch dir) → refetch

    def count_changed(self):
        try:
            out = run_bd_json(self.bd_bin, ["count", "--json"], self.repo)
            return out.get("count") != len(self.cache.issues)
        except BdError:
            return False  # probe failure is not a change signal

    def probe_loop(self):
        while True:
            time.sleep(self.poll_interval)
            self._tick += 1
            changed = self.fingerprint_changed()
            if not changed and self._tick % PROBE_TICKS == 0:
                changed = self.count_changed()
            if not changed:
                continue
            self._refetch()

    def _refetch(self):
        if self._refetching:
            return  # another probe already refetches
        self._refetching = True
        try:
            # fetch outside the lock: bd list takes ~a second and must
            # not block the read endpoints
            with self.lock:
                self.cache.begin_refetch()
            delta = self.cache.refresh_unlocked()
            if delta.get("error"):
                self._publish(delta)
                return  # fingerprint stays armed; next tick retries
            bd_mode = self._probe_mode()
            with self.lock:
                self.cache.commit_refetch()
                self.bd_mode = bd_mode
            self.capture_fingerprint()
            self._publish(delta)
        finally:
            self._refetching = False

    def capture_fingerprint(self):
        self._fingerprints = {}
        for p in self.cache.fingerprint_paths():
            try:
                st = p.stat()
                self._fingerprints[str(p)] = (st.st_mtime_ns, st.st_size)
            except OSError:
                pass

    # ------------------------------------------------------------ SSE fan-out

    def subscribe(self):
        """Register an SSE listener entry {q, rev}; rev = snapshot revision."""
        q = queue.Queue(maxsize=100)
        entry = {"q": q, "rev": 0}
        with self.lock:
            entry["rev"] = self.cache.revision
            self.listeners.append(entry)
        return entry

    def unsubscribe(self, entry):
        with self.lock:
            if entry in self.listeners:
                self.listeners.remove(entry)

    def _publish(self, delta):
        with self.lock:
            revision = self.cache.revision
            event = {
                "revision": revision,
                "upserted": delta["upserted"],
                "removed": delta["removed"],
            }
            if delta.get("error"):
                event = {"revision": revision, "stale": True}
            for entry in list(self.listeners):
                try:
                    entry["q"].put_nowait(event)
                except queue.Full:
                    pass  # a stalled client misses patches; reset covers it

def journal_available(bd_bin, repo):
    """The journal records only when events-journal is enabled for the
    workspace; enabling it mutates shared DB state, so we only detect."""
    try:
        proc = run_bd(bd_bin, ["events", "tail", "--since", "0"], repo)
        return "disabled for this workspace" not in proc.stderr
    except BdError:
        return False


def journal_loop(watcher, bd_bin, repo):
    """Follow the events journal; each record means one snapshot refresh."""
    while True:
        try:
            proc = subprocess.Popen(
                [bd_bin, "events", "tail", "--since", "0", "--follow"],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                text=True,
                cwd=str(repo),
            )
            for _line in proc.stdout:
                watcher._refetch()
        except Exception:
            pass
        time.sleep(watcher.poll_interval)  # restart the tail on exit


def make_handler(watcher, allow_write):
    class BeadsHandler(SimpleHTTPRequestHandler):
        def do_GET(self):
            path = urlparse(self.path).path
            if path == "/api/beads":
                revision, issues = watcher.snapshot()
                self._send_json({"revision": revision, "issues": issues})
            elif match := re.match(r"^/api/beads/issue/([^/]+)$", path):
                self._send_issue(unquote(match.group(1)))
            elif path == "/api/health":
                with watcher.lock:
                    payload = {
                        "status": "ok",
                        "revision": watcher.cache.revision,
                        "issues": len(watcher.cache.issues),
                        "bd_mode": watcher.bd_mode,
                        "error_count": watcher.cache.error_count,
                        "poll_interval": watcher.poll_interval,
                    }
                self._send_json(payload)
            elif path == "/api/indices":
                with watcher.lock:
                    payload = watcher.cache.indices_payload()
                self._send_json(payload)
            elif path == "/api/events":
                self._send_events()
            elif path == "/api/capabilities":
                self._send_json({"write": allow_write})
            else:
                super().do_GET()

        # ---------------------------------------------------------------- SSE

        def _send_events(self):
            """SSE stream: hello, patch, reset, stale, heartbeats.

            The client's last seen revision arrives as ?rev=; anything
            older than the retained history gets a `reset` (full refetch)
            instead of replayed patches.
            """
            query = parse_qs(urlparse(self.path).query)
            try:
                client_rev = int(query.get("rev", ["0"])[0])
            except ValueError:
                client_rev = 0
            entry = watcher.subscribe()
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.send_header("Connection", "keep-alive")
            self.end_headers()
            self.wfile.write(b"retry: 2000\n\n")

            lock = threading.Lock()  # one stalled client must not wedge others

            def send(event_type, data):
                with lock:
                    self.wfile.write(
                        f"event: {event_type}\ndata: {json.dumps(data)}\n\n".encode()
                    )

            try:
                revision, issues = watcher.snapshot()
                if client_rev and client_rev != revision:
                    # gap (missed patches) or server restart (rewound
                    # revision) — either way the client must refetch
                    send("reset", {"revision": revision})
                else:
                    send("hello", {"revision": revision})
                entry["rev"] = revision
                while True:
                    try:
                        event = entry["q"].get(timeout=SSE_HEARTBEAT)
                    except queue.Empty:
                        event = None
                    if event is None:
                        with lock:
                            self.wfile.write(b": heartbeat\n\n")
                    elif event.get("stale"):
                        send("stale", event)
                    elif event["revision"] - entry["rev"] > 1:
                        # gap: patches between the client's revision and now
                        # were lost (queue full / slow client) → full refetch
                        send("reset", {"revision": event["revision"]})
                        entry["rev"] = event["revision"]
                    else:
                        send("patch", event)
                        entry["rev"] = event["revision"]
            except (BrokenPipeError, ConnectionResetError, OSError):
                pass
            finally:
                watcher.unsubscribe(entry)

        # ------------------------------------------------------------- helpers

        def _send_issue(self, issue_id):
            with watcher.lock:
                issue = watcher.cache.issue(issue_id, with_comments=True)
            if issue is None:
                self.send_error(404, f"unknown bead {issue_id}")
            else:
                self._send_json({"revision": watcher.cache.revision, "issue": issue})

        def _send_json(self, obj):
            body = json.dumps(obj).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, fmt, *args):
            pass

        def log_error(self, fmt, *args):
            sys.stderr.write("%s - %s\n" % (self.address_string(), fmt % args))

    return BeadsHandler


def serve(host, port, watcher, allow_write):
    handler = make_handler(watcher, allow_write)
    httpd = ThreadingHTTPServer((host, port), lambda *a, **kw: handler(*a, directory=HERE, **kw))
    httpd.daemon_threads = True
    threading.Thread(target=watcher.probe_loop, daemon=True).start()
    print(
        f"beads-server: serving {watcher.repo} ({len(watcher.cache.issues)} beads) "
        f"on http://{host}:{port}",
        file=sys.stderr,
    )
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass


def main():
    parser = argparse.ArgumentParser(description=__doc__, prog="beads-server")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8339)
    parser.add_argument("--repo", default=os.getcwd())
    parser.add_argument("--bd", default="bd")
    parser.add_argument("--allow-write", action="store_true")
    parser.add_argument(
        "--poll-interval",
        type=float,
        default=2.0,
        help="seconds between change probes (default: 2)",
    )
    parser.add_argument(
        "--events-journal",
        action="store_true",
        help="tail the bd events journal instead of stat polling "
        "(needs events-journal enabled in the workspace; falls back to polling)",
    )
    args = parser.parse_args()

    repo = Path(args.repo).resolve()
    if not (repo / ".beads").is_dir():
        sys.exit(f"error: {repo}/.beads does not exist. Pass the repo with --repo.")
    watcher = Watcher(args.bd, repo, args.poll_interval)
    if args.events_journal and journal_available(args.bd, repo):
        threading.Thread(
            target=journal_loop, args=(watcher, args.bd, repo), daemon=True
        ).start()
        watcher.polling = False
    serve(args.host, args.port, watcher, args.allow_write)


if __name__ == "__main__":
    main()
