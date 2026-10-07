# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

"""Check the generated init without mounts or writes to the host store."""

import argparse
import os
from pathlib import Path
import shlex
import sqlite3
import subprocess
import tempfile
import unittest


SCHEMA = """
CREATE TABLE ValidPaths (id INTEGER PRIMARY KEY, path TEXT UNIQUE NOT NULL);
CREATE TABLE Refs (
    referrer INTEGER NOT NULL REFERENCES ValidPaths(id) ON DELETE CASCADE,
    reference INTEGER NOT NULL REFERENCES ValidPaths(id) ON DELETE RESTRICT,
    PRIMARY KEY (referrer, reference)
);
"""


class InitTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="mysbx-rootfs-test-", dir="/tmp")
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name)
        self.store = self.base / "nix/store"
        self.source = self.base / "stage-ro/nix-var/db"
        self.scratch = self.base / "mysbx-nix"
        for directory in (self.store, self.source, self.scratch):
            directory.mkdir(parents=True)
        self.source_db = self.source / "db.sqlite"
        self.db = self.scratch / "state/db/db.sqlite"

    def seed(self, names, refs=()):
        with sqlite3.connect(self.source_db) as db:
            db.executescript(SCHEMA)
            db.executemany(
                "INSERT INTO ValidPaths VALUES (?, ?)",
                enumerate((str(self.store / name) for name in names), start=1),
            )
            db.executemany("INSERT INTO Refs VALUES (?, ?)", refs)

    def run_reconcile(self, fail_query=None):
        # Extract the init's complete copy/reconcile block, not a SQL replica.
        start = INIT.index("    if [ -d /tmp/mysbx-shares/stage-ro/nix-var/db ]; then")
        reconcile = INIT.index("    if [ -f /mysbx-nix/state/db/db.sqlite ]; then", start)
        end = INIT.index("\n    fi\n", reconcile) + len("\n    fi\n")
        block = INIT[start:end]
        self.assertNotRegex(block, r'"\$BB"\s+(?:mount|umount|chroot)\b|/bin/mkfs')
        # No guest absolute path is touched. SQL and filesystem probes use the
        # same private store, while the baked static executables stay unchanged.
        for guest, local in (
            ("/tmp/mysbx-shares/stage-ro/nix-var/db", str(self.source)),
            ("/mysbx-nix", str(self.scratch)),
            ("/nix/store", str(self.store)),
        ):
            block = block.replace(guest, local)
        sqlite = ROOTFS / "bin/sqlite3"
        if fail_query:
            shim = self.base / "sqlite-failure"
            shim.write_text(
                f"#!{ROOTFS}/bin/busybox sh\n"
                'case "$*" in\n'
                f"    *{shlex.quote(fail_query)}*)\n"
                f"        printf '%s\\n' {shlex.quote(str(self.store / 'missing'))}\n"
                "        exit 1 ;;\n"
                "esac\n"
                f'exec {shlex.quote(str(sqlite))} "$@"\n'
            )
            shim.chmod(0o700)
            sqlite = shim
        block = block.replace("/bin/sqlite3", shlex.quote(str(sqlite)))
        script = (
            "set -eu\n"
            f"BB={shlex.quote(str(ROOTFS / 'bin/busybox'))}\n"
            'fail() { echo "mysbx-init: $*" >&2; exit 125; }\n'
            'step() { echo "mysbx-init: $*"; }\n'
            + block
            + '\necho "mysbx-test: overlay boundary reached"\n'
        )
        return subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-c", script],
            capture_output=True,
            text=True,
            env={"PATH": ""},
            timeout=20,
        )

    def rows(self, path=None):
        with sqlite3.connect(path or self.db) as db:
            return (
                db.execute("SELECT id, path FROM ValidPaths ORDER BY id").fetchall(),
                db.execute("SELECT * FROM Refs ORDER BY referrer, reference").fetchall(),
            )

    def assert_failed(self, result, message):
        self.assertEqual(result.returncode, 125, result.stdout + result.stderr)
        self.assertIn(message, result.stderr)
        self.assertNotIn("copied nix db validation complete", result.stdout)
        self.assertNotIn("mysbx-test: overlay boundary reached", result.stdout)

    def test_init_parses(self):
        result = subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-n"],
            input=INIT,
            text=True,
            capture_output=True,
            timeout=20,
        )
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_reconcile_precedes_overlay_setup(self):
        phases = [
            'done < "$records_file"',
            '/bin/mkfs.ext4 -q -F "$dev"',
            '"$BB" mount -t ext4 "$dev" /mysbx-nix',
            '"$BB" cp -R "$f" /mysbx-nix/state/db/',
            "SELECT path FROM ValidPaths WHERE path LIKE",
            "DELETE FROM Refs WHERE reference =",
            "DELETE FROM ValidPaths WHERE path =",
            "PRAGMA foreign_key_check;",
            'fail "foreign-key violations remain in the copied nix database"',
            '"$BB" mkdir -p /mysbx-nix/store-upper /mysbx-nix/store-work',
            '"$BB" mount -t tmpfs tmpfs /nix \\\n',
            '"$BB" mkdir -p /nix/store',
            '"$BB" mount -t overlay overlay',
            'exec "$@"',
        ]
        offsets = [INIT.index(phase) for phase in phases]
        self.assertEqual(offsets, sorted(offsets), "DB validation must precede /nix shadowing and the overlay")
        self.assertEqual(INIT.count('"$BB" mount -t overlay overlay'), 1)
        self.assertEqual(INIT.count('"$BB" mount -t tmpfs tmpfs /nix \\\n'), 1)
        self.assertEqual(
            os.readlink(ROOTFS / "nix/store"),
            "/tmp/mysbx-shares/stage-ro/store",
        )

    def test_trace_phases(self):
        phases = [
            'step "validating the copied nix db against the read-only store share"',
            "SELECT path FROM ValidPaths WHERE path LIKE",
            'fail "foreign-key violations remain in the copied nix database"',
            'step "copied nix db validation complete"',
            'step "mounting the store overlay (fresh scratch upper)"',
            '"$BB" mount -t overlay overlay',
        ]
        offsets = [INIT.index(phase) for phase in phases]
        self.assertEqual(offsets, sorted(offsets))

    def test_prunes_missing_and_both_symlinks_with_incoming_refs(self):
        (self.store / "directory").mkdir()
        (self.store / "file").write_text("valid store object")
        (self.store / "live-link").symlink_to("directory")
        (self.store / "broken-link").symlink_to("absent")
        names = ["directory", "file", "missing", "live-link", "broken-link"]
        refs = [(1, 3), (1, 4), (2, 5), (3, 1), (4, 3), (2, 1)]
        self.seed(names, refs)
        with sqlite3.connect(self.source_db) as db:
            db.execute("INSERT INTO ValidPaths VALUES (6, '/not-a-store-path')")
            db.execute("INSERT INTO Refs VALUES (6, 3)")
        before = self.rows(self.source_db)
        result = self.run_reconcile()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("3 registered store path(s) missing or symlinked", result.stdout)
        self.assertIn("copied nix db validation complete", result.stdout)
        self.assertIn("mysbx-test: overlay boundary reached", result.stdout)
        paths, refs = self.rows()
        self.assertEqual([row[0] for row in paths], [1, 2, 6])
        self.assertEqual(refs, [(2, 1)])
        with sqlite3.connect(self.db) as db:
            self.assertEqual(db.execute("PRAGMA foreign_key_check").fetchall(), [])
        self.assertEqual(self.rows(self.source_db), before, "the shared source DB must not change")
        for name in ("registered-paths.txt", "doomed-paths.txt"):
            self.assertFalse((self.scratch / name).exists())
        self.assertTrue((self.store / "live-link").is_symlink())
        self.assertTrue((self.store / "broken-link").is_symlink())

    def test_valid_rows_need_no_delete_transaction(self):
        (self.store / "directory").mkdir()
        self.seed(["directory"], [(1, 1)])
        # A delete transaction supplies SQL on stdin, so reject its -bail argv.
        result = self.run_reconcile(fail_query="-bail")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.rows(), self.rows(self.source_db))

    def test_no_database_stays_optional(self):
        self.source.rmdir()
        result = self.run_reconcile()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("no nix-var db", result.stdout)
        self.assertFalse(self.db.exists())
        self.assertNotIn("copied nix db validation complete", result.stdout)

    def test_enumeration_failure_with_partial_output_is_closed(self):
        self.seed(["missing"])
        result = self.run_reconcile(fail_query="SELECT path FROM ValidPaths")
        self.assert_failed(result, "cannot enumerate registered store paths")
        self.assertEqual(self.rows(), self.rows(self.source_db))

    def test_delete_failure_rolls_back_refs_and_paths(self):
        (self.store / "directory").mkdir()
        self.seed(["directory", "missing"], [(1, 2), (2, 1)])
        with sqlite3.connect(self.source_db) as db:
            db.executescript(
                "CREATE TRIGGER reject_delete BEFORE DELETE ON ValidPaths "
                "BEGIN SELECT RAISE(ABORT, 'test delete failure'); END;"
            )
        result = self.run_reconcile()
        self.assert_failed(result, "cannot reconcile dependent rows")
        self.assertEqual(self.rows(), self.rows(self.source_db))

    def test_foreign_key_query_failure_is_closed(self):
        (self.store / "directory").mkdir()
        self.seed(["directory"])
        result = self.run_reconcile(fail_query="PRAGMA foreign_key_check;")
        self.assert_failed(result, "cannot check foreign keys")

    def test_foreign_key_violations_are_closed(self):
        (self.store / "directory").mkdir()
        self.seed(["directory"], [(1, 999)])
        result = self.run_reconcile()
        self.assert_failed(result, "foreign-key violations remain")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("rootfs", type=Path)
    parser.add_argument("--init", type=Path, help="override only the init text for a baseline comparison")
    args, remaining = parser.parse_known_args()
    ROOTFS = args.rootfs.resolve()
    INIT = (args.init or ROOTFS / "bin/mysbx-init").read_text()
    unittest.main(argv=[__file__, *remaining])
