#!/usr/bin/env python3
"""beads-server: kanban board over the bd (beads) issue DB.

Serves the static frontend plus a JSON API over a cached `bd list
--all` snapshot: board, graph and details pane, `/api/health`,
`/api/capabilities`. A fingerprint probe thread refetches the
snapshot when the underlying Dolt files change.

Usage:
    server.py [--host HOST] [--port PORT] [--repo PATH]
"""

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

import argparse
import json
import os
import re
import sys
import threading
import time
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import unquote, urlparse

sys.path.insert(0, str(Path(__file__).resolve().parent))
from bdcache import BdCache, BdError, run_bd  # noqa: E402

HERE = Path(__file__).resolve().parent / "static"

FINGERPRINT_INTERVAL = 2.0


class App:
    def __init__(self, bd_bin, repo, allow_write):
        self.bd_bin = bd_bin
        self.repo = repo
        self.allow_write = allow_write
        self.cache = BdCache(bd_bin, repo)
        self.lock = threading.Lock()
        self._fingerprints = {}
        self.bd_mode = get_bd_mode(bd_bin, repo)

    def snapshot(self):
        """Consistent (revision, issues) pair, safe against probe swaps."""
        with self.lock:
            return self.cache.revision, self.cache.issues

    def refresh(self):
        with self.lock:
            self.cache.refresh()
            self.bd_mode = get_bd_mode(self.bd_bin, self.repo)

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

    def probe_loop(self):
        while True:
            time.sleep(FINGERPRINT_INTERVAL)
            try:
                if self.fingerprint_changed():
                    self.refresh()
                    self.capture_fingerprint()
            except BdError:
                pass  # bd transient failure; keep serving the old snapshot

    def capture_fingerprint(self):
        self._fingerprints = {}
        for p in self.cache.fingerprint_paths():
            try:
                st = p.stat()
                self._fingerprints[str(p)] = (st.st_mtime_ns, st.st_size)
            except OSError:
                pass


def get_bd_mode(bd_bin, repo):
    """Which workspace resolution bd accepts on this repo."""
    try:
        run_bd(bd_bin, ["count", "--json"], repo)
        return "workspace-discovery"
    except BdError:
        return "beads-dir-fallback"


def make_handler(app):
    class BeadsHandler(SimpleHTTPRequestHandler):
        def do_GET(self):
            path = urlparse(self.path).path
            if path == "/api/beads":
                revision, issues = app.snapshot()
                self._send_json({"revision": revision, "issues": issues})
            elif match := re.match(r"^/api/beads/issue/([^/]+)$", path):
                self._send_issue(unquote(match.group(1)))
            elif path == "/api/health":
                self._send_json(
                    {
                        "status": "ok",
                        "revision": app.cache.revision,
                        "issues": len(app.cache.issues),
                        "bd_mode": app.bd_mode,
                    }
                )
            elif path == "/api/indices":
                with app.lock:
                    payload = app.cache.indices_payload()
                self._send_json(payload)
            elif path == "/api/capabilities":
                self._send_json({"write": app.allow_write})
            else:
                super().do_GET()

        def _send_issue(self, issue_id):
            with app.lock:
                issue = app.cache.issue(issue_id, with_comments=True)
            if issue is None:
                self.send_error(404, f"unknown bead {issue_id}")
            else:
                self._send_json({"revision": app.cache.revision, "issue": issue})

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


def serve(host, port, app):
    handler = make_handler(app)
    httpd = ThreadingHTTPServer((host, port), lambda *a, **kw: handler(*a, directory=HERE, **kw))
    httpd.daemon_threads = True
    app.capture_fingerprint()
    threading.Thread(target=app.probe_loop, daemon=True).start()
    print(
        f"beads-server: serving {app.repo} ({len(app.cache.issues)} beads) "
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
    args = parser.parse_args()

    repo = Path(args.repo).resolve()
    if not (repo / ".beads").is_dir():
        sys.exit(f"error: {repo}/.beads does not exist. Pass the repo with --repo.")
    app = App(args.bd, repo, args.allow_write)
    serve(args.host, args.port, app)


if __name__ == "__main__":
    main()
