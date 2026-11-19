# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
"""bd subprocess access, snapshot cache and derived indices.

The only data path is the `bd` CLI (never Dolt/SQL, never
`.beads/*.jsonl` — upstream documents that file as a passive export).
One bulk read serves the whole board; details are fetched lazily and
cached on the issue dict (a refresh rebuilds every dict, which
invalidates the cache per revision).
"""

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
    """Snapshot of all issues, refreshed wholesale on fingerprint change."""

    def __init__(self, bd_bin, repo):
        self.bd_bin = bd_bin
        self.repo = repo
        self.revision = 0
        self.issues = []
        self.by_id = {}
        self.refresh()

    def refresh(self):
        issues = run_bd_json(self.bd_bin, ["list", "--all", "--limit", "0", "--json"], self.repo)
        self.issues = issues
        self.by_id = {i["id"]: i for i in issues}
        self.revision += 1

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
