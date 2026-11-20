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
import copy
import json
import os
import queue
import re
import secrets
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
ACTIVITY_CAP = 500         # retained activity-feed entries

# fields shown in /api/activity and the history popup; description & co
# are diffed as changed/exist, their text never rides along
ACTIVITY_FIELDS = (
    "status", "title", "priority", "assignee", "issue_type", "type",
    "parent", "close_reason", "labels",
)
HISTORY_FIELDS = ACTIVITY_FIELDS + ("description", "notes", "acceptance_criteria")


def _field_changes(old, new, fields):
    """Field-level diff of two issue dicts over `fields`, preserving
    issue order: `labels` compares as sorted lists, everything else
    with plain (in)equality. Returns [{field, from, to}]."""
    changes = []
    for field in fields:
        a = old.get(field) if old else None
        b = new.get(field)
        if field == "labels":
            a = sorted(a or [])
            b = sorted(b or [])
        if a != b:
            changes.append({"field": field, "from": a, "to": b})
    return changes


def diff_history(entries, fields):
    """Reduce bd's per-commit snapshots to field-level changes.

    entries is `bd history <id> --json` — one full issue snapshot per
    commit, newest first. Each output entry shows what its commit
    actually changed, diffed against the successor (the next list
    element); the oldest entry is the creation. Enrichment-only
    rewrites leave no diffed change and are dropped, except the
    creation commit.

    bd history omits bd list fields (dependencies, owner) and reports
    created_at/updated_at as zero timestamps — only `fields` are
    diffed, so none of that leaks in as a change.
    """
    out = []
    for i, entry in enumerate(entries):
        new = entry.get("Issue") or {}
        old = entries[i + 1].get("Issue") if i + 1 < len(entries) else None
        changes = _field_changes(old, new, fields)
        if not changes and old is not None:
            continue  # no-op commit: rewrote the record without touching diffed fields
        out.append({
            "commit": entry.get("CommitHash", ""),
            "date": entry.get("CommitDate", ""),
            "committer": entry.get("Committer", ""),
            "issue": new.get("id"),
            "changes": changes,
        })
    return out


def issue_history(bd_bin, repo, issue_id):
    """`bd history <id> --json` as field-level changes, or None.

    bd prints `null` (not `[]`) for ids with no commit history, and
    exits 0 for unknown ids likewise; both map to None here.
    """
    try:
        raw = run_bd_json(
            bd_bin, ["history", issue_id, "--json"], repo, timeout=120
        )
    except BdError:
        return None
    if not isinstance(raw, list):
        return None
    return diff_history(raw, HISTORY_FIELDS)



class Watcher:
    """Owns the snapshot cache; probes cheap, refetches single-flight."""

    def __init__(self, bd_bin, repo, poll_interval):
        self.bd_bin = bd_bin
        self.repo = repo
        self.poll_interval = poll_interval
        self.token = secrets.token_urlsafe(32)
        # generation identifies THIS server process: a restart resets
        # the revision counter, and clients must not treat the new
        # process's revisions as comparable to the old ones
        self.generation = secrets.token_urlsafe(8)
        self.cache = BdCache(bd_bin, repo)
        self.lock = threading.Lock()
        self._cycle_lock = threading.Lock()  # serializes whole refetches
        self._fingerprints = {}
        self._tick = 0
        self._refetching = False  # single-flight guard
        self.bd_mode = self._probe_mode()
        self.listeners = []  # {"q": Queue, "rev": int} per SSE client
        # activity feed since server start, newest first, capped at
        # ACTIVITY_CAP; written under self.lock
        self.activity = []
        self._known_fields = {}  # id -> {ACTIVITY_FIELDS} of the live snapshot
        self.started = time.time()
        # baseline for field diffs; the start snapshot itself is NOT
        # activity — the feed shows only changes observed after start
        for issue in self.cache.issues:
            self._known_fields[issue["id"]] = {
                f: issue.get(f) for f in ACTIVITY_FIELDS
            }
        self.capture_fingerprint()

    def _probe_mode(self):
        try:
            run_bd(self.bd_bin, ["count", "--json"], self.repo)
            return "workspace-discovery"
        except BdError:
            return "beads-dir-fallback"

    def snapshot(self):
        """Consistent (generation, revision, issues) triple."""
        with self.lock:
            return self.generation, self.cache.revision, self.cache.issues

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

    def _refetch(self, force=False):
        # the cycle lock serializes fetch→commit→publish: two refetches
        # share the cache's pending-snapshot and delta slots, so letting
        # one run mid-cycle commits the other's snapshot and publishes
        # the wrong delta under that revision
        if self._refetching and not force:
            return  # another probe already refetches
        with self._cycle_lock:
            if self._refetching and not force:
                return  # a cycle started while we waited for the lock
            self._refetching = True
            try:
                # fetch outside the cache lock: bd list takes ~a second
                # and must not block the read endpoints
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
                    self.record_activity(delta)
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

    # --------------------------------------------------------- activity feed

    def record_activity(self, delta):
        """Turn a committed delta into feed entries (newest first).

        Called from _refetch after commit_refetch(), before _publish,
        under self.lock. The field diff is coarser than bd history's
        (dependencies are not diffed — lane/blocked state is derived
        and reaches clients as derived upserts — and there is no commit
        identity), so entries carry revision and wall-clock time.
        """
        if delta.get("error"):
            return
        now = time.time()
        seen = set()  # the derived-completion duplicate of an upsert
        for issue in delta.get("upserted", []):
            if issue["id"] in seen:
                continue
            seen.add(issue["id"])
            known = self._known_fields.get(issue["id"])
            if known is None:
                kind = "created"
                changes = []
            else:
                changes = _field_changes(known, issue, ACTIVITY_FIELDS)
                kind = "changed" if changes else "touched"
            self._known_fields[issue["id"]] = {
                f: issue.get(f) for f in ACTIVITY_FIELDS
            }
            self.activity.insert(0, {
                "revision": self.cache.revision,
                "time": now,
                "id": issue["id"],
                "title": issue.get("title", ""),
                "kind": kind,
                "changes": changes,
            })
        for issue_id in delta.get("removed", []):
            self._known_fields.pop(issue_id, None)
            self.activity.insert(0, {
                "revision": self.cache.revision,
                "time": now,
                "id": issue_id,
                "title": "",
                "kind": "deleted",
                "changes": [],
            })
        del self.activity[ACTIVITY_CAP:]

    # ------------------------------------------------------------ SSE fan-out

    def subscribe(self):
        """Register an SSE listener entry {q, rev, gen}."""
        q = queue.Queue(maxsize=100)
        entry = {"q": q, "rev": 0, "gen": None}
        with self.lock:
            entry["rev"] = self.cache.revision
            entry["gen"] = self.generation
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
                "generation": self.generation,
                "revision": revision,
                "upserted": delta["upserted"],
                "removed": delta["removed"],
            }
            if delta.get("error"):
                event = {"generation": self.generation, "revision": revision, "stale": True}
            else:
                # derived fields + indices ride along so a patch keeps
                # the client consistent without an extra round trip
                self.cache.enrich_delta(delta)
                event["indices"] = delta["indices"]
                event["indices"]["generation"] = self.generation
            for entry in list(self.listeners):
                try:
                    entry["q"].put_nowait(event)
                except queue.Full:
                    pass  # a stalled client misses patches; reset covers it

def run_bd_input(bd_bin, args, repo, stdin_text, timeout=600):
    """Like run_bd, but feeds stdin_text (bd --*-file - reads it) and
    attributes the write to beads-server."""
    env = {**os.environ, "BEADS_ACTOR": "beads-server"}
    env.pop("BEADS_DIR", None)
    for beads_dir in (None, str(repo / ".beads")):
        if beads_dir is not None:
            env["BEADS_DIR"] = beads_dir
        proc = subprocess.run(
            [bd_bin, *args],
            input=stdin_text,
            capture_output=True,
            text=True,
            cwd=str(repo),
            env=env,
            timeout=timeout,
        )
        if proc.returncode == 0:
            return proc
        if beads_dir is None and "no beads database found" not in proc.stderr \
                and "cannot resolve workspace" not in proc.stderr:
            break
    return proc


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
        def end_headers(self):
            # static files live in the Nix store, where every mtime is
            # 1: a Last-Modified-only response is heuristically cached
            # "forever" and even revalidation 304s against the new build.
            self.send_header("Cache-Control", "no-store")
            super().end_headers()

        def check_loopback_host(self):
            """DNS rebinding defense for EVERY request, reads included:
            without a Host check a rebinding attacker becomes
            same-origin and can read issue data and the session token
            from /api/session even on a loopback bind."""
            host = (self.headers.get("Host") or "").rsplit(":", 1)[0]
            if host not in ("127.0.0.1", "localhost", "[::1]", "::1"):
                self.send_error(403, "non-loopback Host refused")
                return False
            return True

        def do_GET(self):
            if not self.check_loopback_host():
                return
            path = urlparse(self.path).path
            if path == "/api/beads":
                # Capture issues and indices at the same revision, including
                # when the client is recovering across a process restart.
                with watcher.lock:
                    payload = watcher.cache.indices_payload()
                    payload.update(generation=watcher.generation, issues=watcher.cache.issues)
                self._send_json(payload)
            elif match := re.match(r"^/api/beads/issue/([^/]+)$", path):
                self._send_issue(unquote(match.group(1)))
            elif match := re.match(r"^/api/beads/issue/([^/]+)/history$", path):
                self._send_issue_history(unquote(match.group(1)))
            elif path == "/api/activity":
                self._send_activity()
            elif path == "/api/health":
                with watcher.lock:
                    payload = {
                        "status": "ok",
                        "generation": watcher.generation,
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
                    payload["generation"] = watcher.generation
                self._send_json(payload)
            elif path == "/api/events":
                self._send_events()
            elif path == "/api/capabilities":
                self._send_json({"generation": watcher.generation, "write": allow_write})
            elif path == "/api/session" and allow_write:
                self._send_json({"generation": watcher.generation, "token": watcher.token})
            elif path == "/api/session":
                self.send_error(403, "editing disabled: start with --allow-write")
            elif (
                re.fullmatch(r"/[^/]+", path)
                and len(path) > 1
                and not (HERE / path.lstrip("/")).is_file()
            ):
                # deep link like /myconfig-6di.5: serve the SPA shell,
                # the app resolves the id client-side (any unknown
                # single-segment path is a candidate bead id, the pane
                # shows "unknown bead" if it is not); real static files
                # keep precedence so /app.js etc. never hit this branch
                self.path = "/index.html"
                super().do_GET()
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
            client_gen = query.get("gen", [""])[0]
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
                generation, revision, issues = watcher.snapshot()
                if (
                    client_gen and client_gen != generation
                ) or (client_rev and client_rev != revision):
                    # foreign generation (server restarted, revisions
                    # not comparable) or revision gap (missed patches)
                    # — either way the client must refetch
                    send("reset", {"generation": generation, "revision": revision})
                else:
                    send("hello", {"generation": generation, "revision": revision})
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
                        send("reset", {"generation": generation, "revision": event["revision"]})
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

        def _send_issue_history(self, issue_id):
            """Field-level history of one bead, straight from bd — no
            lock held: the db is immutable history, and a lock-held bd
            subprocess would stall every read endpoint for ~0.5 s."""
            with watcher.lock:
                known = issue_id in watcher.cache.by_id
            if not known:
                self.send_error(404, f"unknown bead {issue_id}")
                return
            history = issue_history(watcher.bd_bin, watcher.repo, issue_id)
            self._send_json({
                "generation": watcher.generation,
                "revision": watcher.cache.revision,
                "id": issue_id,
                "history": history,
            })

        def _send_activity(self):
            with watcher.lock:
                activity = copy.copy(watcher.activity)
                started = watcher.started
            self._send_json({
                "generation": watcher.generation,
                "revision": watcher.cache.revision,
                "since": started,
                "activity": activity,
            })

        # ---------------------------------------------------------- writing

        # argv allowlist per field; values are passed as single argv
        # entries, never through a shell
        UPDATE_FLAGS = {
            "status": "--status",
            "priority": "--priority",
            "assignee": "--assignee",
            "type": "--type",
            "title": "--title",
            "parent": "--parent",
        }
        UPDATE_STDIN = {
            "description": "--body-file",
            "design": "--design-file",
        }

        def do_POST(self):
            # Origin + Host checks: the Host check above covers reads;
            # writes additionally refuse any cross-origin request
            if not self.check_loopback_host():
                return
            origin = self.headers.get("Origin") or ""
            if origin:
                ohost = urlparse(origin).hostname or ""
                if ohost not in ("127.0.0.1", "localhost", "::1"):
                    self.send_error(403, "cross-origin write refused")
                    return
            if not allow_write:
                self.send_error(403, "editing disabled: start with --allow-write")
                return
            supplied = self.headers.get("X-Beads-Server-Token") or ""
            if not secrets.compare_digest(supplied, watcher.token):
                self.send_error(403, "missing or invalid X-Beads-Server-Token")
                return
            path = urlparse(self.path).path
            if match := re.match(r"^/api/beads/issue/([^/]+)/update$", path):
                self._update_issue(unquote(match.group(1)))
            elif match := re.match(r"^/api/beads/issue/([^/]+)/comment$", path):
                self._comment_issue(unquote(match.group(1)))
            elif path == "/api/beads":
                self._create_issue()
            elif path == "/api/beads/deps":
                self._dep_change()
            elif path == "/api/beads/batch":
                self._batch()
            else:
                self.send_error(404, f"unknown POST endpoint {path}")

        def _read_json(self):
            try:
                length = int(self.headers.get("Content-Length", "0"))
                return json.loads(self.rfile.read(length))
            except (ValueError, json.JSONDecodeError):
                return None

        def _write_result(self, proc, issue_id):
            """Map a bd subprocess result to an HTTP response.

            Exit 13 = a lost --if-status/--if-assignee guard → 409 with
            the fresh issue, so the caller reloads instead of overwriting.
            """
            if proc.returncode == 0:
                # The editor uses this revision as an acknowledgement. A poll
                # that started before the write cannot stand in for this read.
                watcher._refetch(force=True)
                with watcher.lock:
                    issue = watcher.cache.by_id.get(issue_id)
                    revision = watcher.cache.revision
                body = {"revision": revision, "issue": issue, "output": proc.stdout.strip()}
                self._send_json(body)
            elif proc.returncode == 13:
                # exit 13 = lost --if-status/--if-assignee guard: force a
                # refetch so the 409 carries what ACTUALLY changed, even
                # when a background probe refetch is in flight
                watcher._refetch(force=True)
                with watcher.lock:
                    issue = watcher.cache.by_id.get(issue_id)
                self._send_json_status(409, {"error": "conflict", "issue": issue})
            else:
                self._send_json_status(
                    400, {"error": proc.stderr.strip() or proc.stdout.strip()}
                )

        def _update_issue(self, issue_id):
            req = self._read_json()
            if req is None:
                self.send_error(400, "invalid JSON body")
                return
            args = ["update", issue_id]
            stdin_keys = [k for k in self.UPDATE_STDIN if k in req]
            if len(stdin_keys) > 1:
                # one stdin pipe cannot carry two --*-file - flags
                self.send_error(400, "one stdin field per request")
                return
            for key, flag in self.UPDATE_FLAGS.items():
                if key in req:
                    args += [flag, str(req[key])]
            for key, flag in self.UPDATE_STDIN.items():
                if key in req:
                    args += [flag, "-"]
            if "notes" in req:
                args += ["--notes", str(req["notes"])]
            if "append_notes" in req:
                args += ["--append-notes", str(req["append_notes"])]
            if "acceptance" in req:
                args += ["--acceptance", str(req["acceptance"])]
            for label in req.get("add_labels", []):
                args += ["--add-label", str(label)]
            for label in req.get("remove_labels", []):
                args += ["--remove-label", str(label)]
            if "set_labels" in req:
                args += ["--set-labels", ",".join(str(l) for l in req["set_labels"])]
            if "if_status" in req:
                args += ["--if-status", str(req["if_status"])]
            if "if_assignee" in req:
                args += ["--if-assignee", str(req["if_assignee"])]
            if len(args) == 2:
                self.send_error(400, "no updates specified")
                return
            stdin_text = str(req[stdin_keys[0]]) if stdin_keys else ""
            proc = run_bd_input(watcher.bd_bin, args, watcher.repo, stdin_text)
            self._write_result(proc, issue_id)

        def _comment_issue(self, issue_id):
            req = self._read_json()
            if req is None or not str(req.get("text", "")).strip():
                self.send_error(400, "comment text required")
                return
            proc = run_bd_input(
                watcher.bd_bin, ["comment", issue_id, "--stdin"], watcher.repo,
                str(req["text"]),
            )
            self._write_result(proc, issue_id)

        def _create_issue(self):
            req = self._read_json()
            if req is None or not str(req.get("title", "")).strip():
                self.send_error(400, "title required")
                return
            args = ["create", str(req["title"])]
            if "type" in req:
                args += ["-t", str(req["type"])]
            if "priority" in req:
                args += ["-p", str(req["priority"])]
            if "description" in req:
                args += ["--description-file", "-"]
            for label in req.get("labels", []):
                args += ["--label", str(label)]
            stdin_text = str(req.get("description", ""))
            proc = run_bd_input(watcher.bd_bin, args, watcher.repo, stdin_text)
            self._write_result(proc, None)

        def _dep_change(self):
            req = self._read_json()
            if req is None or not all(k in req for k in ("action", "from", "to")):
                self.send_error(400, "action/from/to required")
                return
            sub = "add" if req["action"] == "add" else "remove"
            args = ["dep", sub, str(req["from"]), str(req["to"])]
            if sub == "add" and "type" in req:
                args += ["--type", str(req["type"])]
            proc = run_bd_input(watcher.bd_bin, args, watcher.repo, "")
            self._write_result(proc, req["from"])

        def _batch(self):
            req = self._read_json()
            if req is None or not isinstance(req.get("commands", []), list):
                self.send_error(400, "commands list required")
                return
            for line in req["commands"]:
                # bd batch parses its own grammar from stdin (no shell),
                # but refuse quotes and metacharacters anyway so a hostile
                # client cannot smuggle a second bd command line
                if re.search(r"[;&|`$\\><\"'\r\n]", str(line)):
                    self.send_error(400, "refusing batch line")
                    return
            stdin_text = "\n".join(str(c) for c in req["commands"])
            proc = run_bd_input(watcher.bd_bin, ["batch"], watcher.repo, stdin_text)
            self._write_result(proc, None)

        def _send_json_status(self, code, obj):
            self._send_json(obj, code=code)

        def _send_json(self, obj, code=200):
            body = json.dumps(obj).encode()
            self.send_response(code)
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
    if host not in ("127.0.0.1", "localhost", "::1"):
        # loopback bind is the security boundary: the session token is
        # only protected against other machines by not being reachable
        sys.exit(
            f"error: --host {host} is not loopback; the server is "
            "loopback-only. Use an SSH tunnel for remote access."
        )
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
