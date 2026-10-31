#!/usr/bin/env python3
"""beads-viewer: single-page web viewer for the bd (beads) issue tracker.

Serves a static HTML page that renders all beads as a dependency graph
(dagre layout) plus a details pane. Bead data is fetched once at startup
by calling `bd list --all --limit 0 --json` and, for issues with
comments, `bd show <ids...> --json --include-comments`, then exposed at
`GET /api/beads`.

Usage:
    server.py [--host HOST] [--port PORT] [--repo PATH]

    --repo PATH   Repository whose `.beads/` DB is displayed.
                  The path is resolved to an absolute path and passed to
                  `bd` via BEADS_DIR as `<repo>/.beads`. Defaults to the
                  current working directory.

The snapshot logic is kept in fetch_beads() so a periodic refresh loop
could call it again later.
"""

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

import argparse
import json
import os
import re
import subprocess
import sys
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import unquote, urlparse

HERE = Path(__file__).resolve().parent


def run_bd(bd, args, beads_dir):
    try:
        proc = subprocess.run(
            [bd, *args],
            capture_output=True,
            text=True,
            env={**os.environ, "BEADS_DIR": str(beads_dir)},
            timeout=600,
        )
    except FileNotFoundError:
        sys.exit("error: `bd` binary not found on PATH")
    except subprocess.TimeoutExpired:
        sys.exit(f"error: `bd {args[0]}` timed out")
    if proc.returncode != 0:
        sys.exit(f"error: `bd {' '.join(args)}` failed:\n{proc.stderr.strip()}")
    return json.loads(proc.stdout)


def fetch_beads(bd, repo):
    """Collect a full bead snapshot from the DB at `<repo>/.beads`.

    One bulk call returns every issue (all statuses) including
    dependencies, notes, labels and assignee; a second bulk call adds
    the comment bodies for issues that have any.
    """
    beads_dir = Path(repo).resolve() / ".beads"
    if not beads_dir.is_dir():
        sys.exit(
            f"error: {beads_dir} does not exist.\n"
            "Pass the beads repo with --repo (its .beads/ holds the bd DB)."
        )

    issues = run_bd(bd, ["list", "--all", "--limit", "0", "--json"], beads_dir)

    with_comments = [i["id"] for i in issues if i.get("comment_count", 0) > 0]
    if with_comments:
        # Chunked so we stay well below ARG_MAX on large issue sets.
        for chunk_start in range(0, len(with_comments), 100):
            chunk = with_comments[chunk_start : chunk_start + 100]
            shown = run_bd(bd, ["show", *chunk, "--json", "--include-comments"], beads_dir)
            by_id = {s["id"]: s for s in shown}
            for issue in issues:
                full = by_id.get(issue["id"])
                if full is not None:
                    issue["comments"] = full.get("comments", [])

    # Not exposed: `notes` can grow large and repeats `description` content.
    return {"issues": issues}


class BeadsHandler(SimpleHTTPRequestHandler):
    # TODO: re-fetch the snapshot on GET /api/beads (or on a timer) once a
    # dynamic/periodic refresh is wanted; until then the startup snapshot
    # is served as-is.
    api_handler = None  # set by serve()

    def do_GET(self):
        if self.api_handler is None:
            self.send_error(503, "no snapshot available")
            return
        path = urlparse(self.path).path
        if path == "/api/beads":
            self._send_json(self.api_handler())
        elif match := re.match(r"^/api/beads/issue/([^/]+)$", path):
            self._send_issue(unquote(match.group(1)))
        else:
            super().do_GET()

    def _send_issue(self, issue_id):
        issues = self.api_handler()["issues"]
        issue = next((i for i in issues if i["id"] == issue_id), None)
        if issue is None:
            self.send_error(404, f"unknown bead {issue_id}")
        else:
            self._send_json(issue)

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


def serve(host, port, repo, bd):
    snapshot = fetch_beads(bd, repo)

    def api_handler():
        return snapshot

    BeadsHandler.api_handler = staticmethod(api_handler)

    httpd = ThreadingHTTPServer(
        (host, port), lambda *a, **kw: BeadsHandler(*a, directory=HERE, **kw)
    )
    print(
        f"beads-viewer: serving {repo} ({len(snapshot['issues'])} beads) "
        f"on http://{host}:{port}",
        file=sys.stderr,
    )
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass


def main():
    parser = argparse.ArgumentParser(description=__doc__, prog="beads-viewer")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8338)
    parser.add_argument(
        "--repo",
        default=os.getcwd(),
        help="repository whose .beads/ DB is displayed (default: $PWD)",
    )
    parser.add_argument(
        "--bd",
        default="bd",
        help="bd binary to invoke (default: `bd` from PATH)",
    )
    args = parser.parse_args()
    serve(args.host, args.port, args.repo, args.bd)


if __name__ == "__main__":
    main()
