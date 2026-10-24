# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

"""Check the generated init without mounts or writes to the host store."""

import argparse
import base64
import os
from pathlib import Path
import shlex
import sqlite3
import subprocess
import tempfile
import unittest


def base64_of(text):
    return base64.b64encode(text.encode()).decode()


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
        block = block.replace("/bin/store-scan", shlex.quote(str(ROOTFS / "bin/store-scan")))
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

    def test_shares_are_bind_mounted_not_linked(self):
        # The sandbox path of a share is a bind mount of its device slot,
        # so getcwd()/realpath() report the sandbox path, never the
        # /tmp/mysbx-shares backing spelling.
        start = INIT.index('while IFS=" " read -r tag slot sandbox mode; do')
        end = INIT.index('done < "$records_file"', start)
        loop = INIT[start:end]
        # Both mounts target the RESOLVED mountpoint: busybox's remount
        # looks it up in /proc/mounts, which records /tmp/mysbx-home/...
        # for a /mysbx-home/... share (the baked link).
        self.assertIn('mnt=$("$BB" readlink -f "$sandbox")', loop)
        self.assertIn('"$BB" mount -o bind "$sharetarget" "$mnt"', loop)
        self.assertIn('"$BB" mount -o remount,bind,ro "$mnt"', loop)
        self.assertNotRegex(loop, r'"\$BB"\s+ln\b')
        self.assertNotRegex(loop, r'"\$BB"\s+rm\s+-rf\b')

    def test_chdir_exports_the_logical_pwd(self):
        # bd myconfig-mr4: the init pins the chdir record's spelling in
        # the exec environment: cd, then export PWD=<the record>. The
        # shares are bind mounts in the guest, but a parent of the chdir
        # may still be a symlink (the baked /mysbx-home), so this runs
        # the REAL generated init text under the baked static busybox
        # with a symlinked layout (no mounts are possible here) and
        # checks the exported PWD against the physical getcwd.
        start = INIT.index('if [ -n "$manifest_chdir" ]; then')
        end = INIT.index("\nfi\n", start) + len("\nfi\n")
        block = INIT[start:end]
        self.assertIn('export PWD="$manifest_chdir"', block,
                      "the init lost the PWD export of the parity fix")
        self.assertLess(INIT.index('cd "$manifest_chdir"'),
                         INIT.index('export PWD="$manifest_chdir"'),
                         "the PWD export must follow the cd")
        backing = self.base / "stage-rw/workspace"
        backing.mkdir(parents=True)
        logical = self.base / "home/synth/repo"
        logical.parent.mkdir(parents=True)
        os.symlink(backing, logical)
        payload = (
            "set -eu\n"
            'BB=%s\n'
            'manifest_chdir=%s\n'
            'fail() { echo "mysbx-init: $*" >&2; exit 125; }\n'
            'step() { echo "mysbx-init: $*"; }\n'
            + block
            + '\n[ "$PWD" = "$1" ] || { echo "PWD=$PWD want=$1" >&2; exit 9; }\n'
            '\n[ "$(pwd -P)" = "$2" ] || { echo "pwdP=$(pwd -P) want=$2" >&2; exit 9; }\n'
            'echo "mysbx-test: PWD=$PWD physical=$(pwd -P)"\n'
        ) % (
            shlex.quote(str(ROOTFS / "bin/busybox")),
            shlex.quote(str(logical)),
        )
        result = subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-c", payload, "sh",
             str(logical), str(backing)],
            capture_output=True,
            text=True,
            env={"PATH": ""},
            timeout=20,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: PWD=%s" % logical, result.stdout)
        self.assertIn("physical=%s" % backing, result.stdout)

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
        # The traced step names of the db reconcile: the split of the
        # old single "validating" step into enumerate + check (each
        # with its own timing) must keep the ORDER and the completion
        # marker the live validation greps for.
        phases = [
            'step "enumerating the registered store paths from the copied nix db"',
            "SELECT path FROM ValidPaths WHERE path LIKE",
            'fail "foreign-key violations remain in the copied nix database"',
            'step "copied nix db validation complete"',
            'step "mounting the store overlay (fresh scratch upper)"',
            '"$BB" mount -t overlay overlay',
        ]
        offsets = [INIT.index(phase) for phase in phases]
        self.assertEqual(offsets, sorted(offsets))

    def test_inode_watchdog_block(self):
        # bd myconfig-dj4: the guest inode cache pins host launcher fds
        # via FUSE lookup refs, and nothing evicts it without memory
        # pressure — the watchdog bounds it guest-side. This runs the
        # REAL block (sleep replaced by break, /proc redirected to a
        # fake tree) under the baked busybox: over the threshold it
        # drops caches, 0/under-threshold never do, and a
        # custom threshold takes effect.
        start = INIT.index('if [ "${MYSBX_KRUN_INODE_WATCHDOG:-')
        end = INIT.index('step "exec-ing the payload: $1"', start)
        block = INIT[start:end]
        # The block forks and must be waited for; the payload is a
        # sentinel through the same "exec $@" shape the init uses.
        payload = (
            "set -eu\n"
            'BB=%s\n'
            'step() { :; }\n'
            % (shlex.quote(str(ROOTFS / "bin/busybox")))
            + block
            + 'echo "mysbx-test: drops=$("$BB" cat /tmp/iw-drop)"\n'
            'exit 0\n'
        )
        # Redirect the block's /proc paths into a fake tree the test
        # controls, and make one iteration observable (no real sleep).
        # wait: the block forks its loop into the background; the
        # sentinel echo must observe the drop AFTER it happened.
        payload = payload.replace('/proc/sys/fs/inode-state', '"$IW_PROC"/fs/inode-state')
        payload = payload.replace('/proc/sys/vm/drop_caches', '"$IW_PROC"/vm/drop_caches')
        payload = payload.replace('/tmp/mysbx-inode-watchdog.log', '"$IW_PROC"/iw.log')
        payload = payload.replace('"$BB" sleep "$_iw_interval"', 'break')
        payload = payload.replace(
            'echo "mysbx-test: drops=$("$BB" cat /tmp/iw-drop)"',
            'wait\necho "mysbx-test: drops=$("$BB" cat /tmp/iw-drop)"',
        )
        # the fake drop counter file, outside the block's replaced
        # paths but pinned at a fixed spot both halves can reach
        payload = payload.replace('/tmp/iw-drop', '"$IW_PROC"/vm/drop_caches')
        # the block's own stdin is the init's; the fake tree must not
        # shadow the harness's own busybox calls
        self.assertIn('wait\n', payload)

        def run(env, inodes, interval="60"):
            base = self.base / "iw-proc"
            (base / "sys/fs").mkdir(parents=True, exist_ok=True)
            (base / "sys/vm").mkdir(parents=True, exist_ok=True)
            (base / "sys/fs/inode-state").write_text(f"{inodes}\t0\t0\t0\t0\t0\t0\n")
            (base / "sys/vm/drop_caches").write_text("0\n")
            script = (
                'IW_PROC=%s\n' % shlex.quote(str(base / "sys"))
                + (
                    'unset MYSBX_KRUN_INODE_WATCHDOG\n' if env is None
                    else 'MYSBX_KRUN_INODE_WATCHDOG=%s\n' % shlex.quote(env)
                )
                + 'MYSBX_KRUN_INODE_WATCHDOG_INTERVAL=%s\n' % shlex.quote(interval)
                + payload
            )
            return subprocess.run(
                [str(ROOTFS / "bin/busybox"), "sh", "-c", script],
                capture_output=True,
                text=True,
                timeout=20,
                env={"PATH": ""},
            )

        # Default and fallback reclaim well below the launcher's
        # minimum supported host ceiling (65536 fds).
        for env in (None, "", "banana"):
            with self.subTest(env=env):
                result = run(env, 30000)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn("mysbx-test: drops=2", result.stdout)
                result = run(env, 20000)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn("mysbx-test: drops=0", result.stdout)
        # disabled: no drop even over the threshold
        result = run("0", 200000)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: drops=0", result.stdout)
        # under the threshold: no drop
        result = run("20000", 4000)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: drops=0", result.stdout)
        # a custom low threshold takes effect
        result = run("1000", 4000)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: drops=2", result.stdout)
        # a custom high threshold takes effect
        result = run("500000", 200000)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: drops=0", result.stdout)
        # the interval knob parses and defaults on garbage the same
        # way (the sleep interval itself is not observable here — it
        # is replaced by break — but the garbage path must not error)
        result = run("banana", 30000, interval="banana")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: drops=2", result.stdout)

    def watchdog_ticks(self, start, observed, post_drop, env=None, log_bytes=0):
        # Run the REAL watchdog block over several ticks. The sleep
        # becomes `iw_tick || break` (feeds the next observed inode
        # count), and the drop_caches write becomes `iw_drop` (counts
        # the drop and sets the post-drop count the block re-reads).
        start_at = INIT.index('if [ "${MYSBX_KRUN_INODE_WATCHDOG:-')
        end = INIT.index('step "exec-ing the payload: $1"', start_at)
        block = INIT[start_at:end]
        drop = "echo 2 > /proc/sys/vm/drop_caches 2>/dev/null || :"
        sleep = '"$BB" sleep "$_iw_interval"'
        for literal in (drop, sleep, "/proc/sys/fs/inode-state", "/tmp/mysbx-inode-watchdog.log"):
            self.assertIn(literal, block)
        block = block.replace(drop, "iw_drop")
        block = block.replace(sleep, "iw_tick || break")
        block = block.replace("/proc/sys/fs/inode-state", '"$IW"/inode-state')
        block = block.replace("/tmp/mysbx-inode-watchdog.log", '"$IW"/iw.log')
        base = self.base / "iw-ticks"
        base.mkdir(exist_ok=True)
        (base / "inode-state").write_text(f"{start}\t0\t0\t0\t0\t0\t0\n")
        (base / "observed").write_text("".join(f"{n}\n" for n in observed))
        (base / "post").write_text("".join(f"{n}\n" for n in post_drop))
        (base / "drops").write_text("")
        if log_bytes:
            (base / "iw.log").write_text("x" * log_bytes)
        script = (
            "set -eu\n"
            "BB=%s\n"
            "IW=%s\n"
            "step() { :; }\n"
            "unset MYSBX_KRUN_INODE_WATCHDOG_INTERVAL\n"
            % (shlex.quote(str(ROOTFS / "bin/busybox")), shlex.quote(str(base)))
            + (
                "unset MYSBX_KRUN_INODE_WATCHDOG\n" if env is None
                else "MYSBX_KRUN_INODE_WATCHDOG=%s\n" % shlex.quote(env)
            )
            + 'iw_pop() { _v=$("$BB" head -n 1 "$IW/$1"); "$BB" sed -i 1d "$IW/$1"; }\n'
            + 'iw_tick() { iw_pop observed; [ -n "$_v" ] || return 1; echo "$_v" > "$IW"/inode-state; }\n'
            + 'iw_drop() { echo drop >> "$IW"/drops; iw_pop post; echo "$_v" > "$IW"/inode-state; }\n'
            + block
            + "wait\n"
        )
        result = subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-c", script],
            capture_output=True,
            text=True,
            timeout=20,
            env={"PATH": ""},
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stderr, "")
        drops = (base / "drops").read_text().count("drop")
        return drops, (base / "iw.log").read_text(), base

    def test_inode_watchdog_tracks_the_post_drop_baseline(self):
        # bd myconfig-jjfy: about 72k inodes stay in use after a drop.
        # A fixed threshold below that drops on every tick; the
        # baseline-relative trigger drops only after 20000 new inodes.
        drops, log, _ = self.watchdog_ticks(
            100000, [80000, 91000, 95000, 90000], [72000, 73000]
        )
        self.assertEqual(drops, 2, log)
        self.assertEqual(
            log.splitlines()[0], "armed: growth 20000, interval 1s", log
        )
        self.assertIn("drop: inodes 100000 -> 72000, next drop above 92000", log)
        self.assertIn("drop: inodes 95000 -> 73000, next drop above 93000", log)
        self.assertEqual(log.count("drop:"), 2, log)
        # A custom growth is relative to the baseline too.
        drops, log, _ = self.watchdog_ticks(
            6000, [9000, 12000], [1000, 9000], env="5000"
        )
        self.assertEqual(drops, 2, log)
        self.assertIn("drop: inodes 6000 -> 1000, next drop above 6000", log)
        self.assertIn("drop: inodes 9000 -> 9000, next drop above 14000", log)

    def test_inode_watchdog_log_rotates(self):
        drops, log, base = self.watchdog_ticks(
            100000, [], [72000], log_bytes=300000
        )
        self.assertEqual(drops, 1)
        rotated = (base / "iw.log.1").read_text()
        self.assertTrue(rotated.startswith("x" * 1000))
        self.assertIn("armed: growth 20000", rotated)
        self.assertEqual(log.count("\n"), 1, log)
        self.assertIn("drop: inodes 100000 -> 72000", log)

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
        self.assertIn(
            "3 of 5 registered store path(s) missing or symlinked", result.stdout
        )
        self.assertIn("copied nix db validation complete", result.stdout)
        self.assertIn("mysbx-test: overlay boundary reached", result.stdout)
        paths, refs = self.rows()
        self.assertEqual([row[0] for row in paths], [1, 2, 6])
        self.assertEqual(refs, [(2, 1)])
        with sqlite3.connect(self.db) as db:
            self.assertEqual(db.execute("PRAGMA foreign_key_check").fetchall(), [])
        self.assertEqual(self.rows(self.source_db), before, "the shared source DB must not change")
        for name in ("registered-paths.txt", "doomed-paths.txt", "store-scan.txt", "recheck-paths.txt"):
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

    def test_slirp_nic_step_gates_on_the_marker_and_finds_the_driver(self):
        # bd myconfig-k106: the SLiRP NIC config step runs ONLY on the
        # qemu backend's marker (the krun guest has no ethernet at
        # all), finds the interface by DRIVER (virtio_net), and
        # configures qemu's fixed user-netdev defaults. The shape is
        # pinned on the generated text; the driver match runs live
        # under the baked busybox against a synthetic sysfs tree.
        start = INIT.index('if [ "${MYSBX_MICROVM_NET:-}" = "1" ]; then')
        end = INIT.index("# The scratch disk", start)
        block = INIT[start:end]
        self.assertIn("*virtio_net)", block)
        self.assertIn("ip addr add 10.0.2.15/24", block)
        self.assertIn("ip route add default via 10.0.2.2", block)
        self.assertIn("the NIC did not attach", block)
        # `ip link set` — never a bare interface name assumption.
        self.assertNotIn("eth0", block)

        # The interface detection against a synthetic /sys/class/net:
        # one lo (no device dir), one NON-virtio device, one
        # virtio_net — the walk must pick the virtio one.
        net = self.base / "sys/class/net"
        lo = net / "lo"
        lo.mkdir(parents=True)
        other = net / "eth0"
        (other / "device").mkdir(parents=True)
        os.symlink("../../bus/pci/drivers/e1000e", other / "device/driver")
        virtio = net / "ens3"
        (virtio / "device").mkdir(parents=True)
        # The driver symlink's TARGET must exist: the init probes
        # with [ -e ] (follows the link). A real sysfs bind resolves
        # against /sys, but the case match only needs the SPELLING —
        # an absolute fixture target keeps the probe honest.
        (self.base / "sys/bus/virtio/drivers/virtio_net").mkdir(parents=True)
        os.symlink(self.base / "sys/bus/virtio/drivers/virtio_net",
                   virtio / "device/driver")
        detection = block[block.index('_nic=""'):block.index('if [ -n "$_nic" ]; then')]
        detection = detection.replace("/sys/class/net", str(net))
        script = (
            "set -eu\n"
            f"BB={shlex.quote(str(ROOTFS / 'bin/busybox'))}\n"
            + detection
            + 'printf "mysbx-test: nic=%s\\n" "$_nic"\n'
        )
        result = subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-c", script],
            capture_output=True,
            text=True,
            env={"PATH": ""},
            timeout=20,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: nic=ens3", result.stdout,
                      f"the driver match: {result.stdout!r}")

    def test_exit_channel_resolves_the_port_through_sysfs(self):
        # bd myconfig-dnmk: /dev/virtio-ports/<name> is a UDEV
        # symlink a static-busybox guest never has — the init must
        # resolve the port by NAME through /sys/class/virtio-ports/*
        # and write to the devtmpfs node (vport<pdev>p<id>), with an
        # mknod fallback. Runs the REAL generated block under the
        # baked static busybox against a synthetic sysfs + dev tree.
        start = INIT.index("    _exit_port=/dev/virtio-ports/$exit_channel")
        end = INIT.index("    set +e", start)
        block = INIT[start:end]
        self.assertIn('/sys/class/virtio-ports', block)
        self.assertIn('mknod', block)

        sysfs = self.base / "sys/class/virtio-ports"
        other = sysfs / "vport0p1"
        exit_port = sysfs / "vport0p2"
        for d, name in ((other, "not-the-exit-port"), (exit_port, "mysbx-exit")):
            d.mkdir(parents=True)
            (d / "name").write_text(name)
            (d / "dev").write_text("245:2\n")
        dev = self.base / "dev"
        dev.mkdir()
        block = block.replace('/sys/class/virtio-ports', str(sysfs))
        block = block.replace('/dev/', str(dev) + "/")
        script = (
            "set -eu\n"
            f"BB={shlex.quote(str(ROOTFS / 'bin/busybox'))}\n"
            'step() { echo "mysbx-init: $*"; }\n'
            'exit_channel=mysbx-exit\n'
            + block
            + 'printf "mysbx-test: port=%s\\n" "$_exit_port"\n'
            # Two named ports — the resolution must pick the
            # mysbx-exit one, not the first in the glob.
        )
        result = subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-c", script],
            capture_output=True,
            text=True,
            env={"PATH": ""},
            timeout=20,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        # The node did NOT exist in the fixture /dev: the mknod
        # fallback must have created it from the dev file.
        self.assertEqual(result.stdout.strip(),
                         f"mysbx-test: port={dev}/vport0p2",
                         result.stdout + result.stderr)
        self.assertTrue((dev / "vport0p2").exists())

        # With the node already present (the devtmpfs shape on a
        # real guest), the existing node is used.
        (dev / "vport0p2").unlink()
        (dev / "vport0p2").write_text("")
        result = subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-c", script],
            capture_output=True,
            text=True,
            env={"PATH": ""},
            timeout=20,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout.strip(), f"mysbx-test: port={dev}/vport0p2")

        # A name match that does not exist: the fallback path stays
        # (/dev/virtio-ports/<name>), the write fails, the teardown
        # falls through — never a hang.
        (exit_port / "name").write_text("also-not-it\n")
        result = subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-c", script],
            capture_output=True,
            text=True,
            env={"PATH": ""},
            timeout=20,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn(
            f"port={dev}/virtio-ports/mysbx-exit", result.stdout.strip(),
            "an unresolved name keeps the udev spelling as the target",
        )

    def test_exit_channel_runs_the_payload_synchronously(self):
        # bd myconfig-8j3t: a NON-INTERACTIVE ash gives an async
        # (`cmd &`) child /dev/null as stdin (the POSIX async-list
        # rule — live-probed with this very busybox), so the
        # exit-channel branch must run the payload synchronously.
        # The generated init text is checked for the shape, and the
        # REAL busybox proves the stdin difference live: the &-child
        # reads EOF, the synchronous child reads the piped line.
        start = INIT.index('if [ -n "$exit_channel" ]; then')
        end = INIT.index('exec "$@"', start)
        branch = INIT[start:end]
        self.assertNotRegex(branch, r'"\$@"\s*&')
        self.assertIn('"$@"\n    _exit_code=$?', branch)

        payload = (
            'read x\necho "child got: [$x]"\n'
        )
        async_probe = f'({payload}) & wait $!\n'
        sync_probe = f'{payload}'
        for form, expected in ((async_probe, "child got: []"), (sync_probe, "child got: [in]")):
            result = subprocess.run(
                [str(ROOTFS / "bin/busybox"), "sh", "-c", form],
                input="in\n",
                capture_output=True,
                text=True,
                timeout=20,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn(expected, result.stdout,
                          f"the {form!r} probe: {result.stdout!r}")

    def test_manifest_argv_records_replace_the_positionals(self):
        # bd myconfig-hq7k: the qemu run carries the payload through
        # the manifest's argv records (the stock kernel's `--`
        # cmdline split is lossy); the loader must replace $* with
        # the decoded tokens, keep spaces/empties whole, refuse a
        # reordered index and refuse a bad base64 — under the REAL
        # generated init text and the baked static busybox.
        start = INIT.index("    _argv_n=0")
        end = INIT.index('    done < "$mpath"', start) + len('    done < "$mpath"\n')
        loader = INIT[start:end]
        self.assertIn('case "$rkind" in', loader)
        self.assertIn("argv)", loader)
        # The extracted block must carry the WHOLE loader: env, argv
        # and the unknown-record refusal.
        self.assertIn("unknown manifest record", loader)

        def run_manifest(records):
            manifest = self.base / "manifest"
            manifest.write_text("".join(records))
            script = (
                "set -eu\n"
                f"BB={shlex.quote(str(ROOTFS / 'bin/busybox'))}\n"
                'fail() { echo "mysbx-init: $*" >&2; exit 125; }\n'
                'step() { echo "mysbx-init: $*"; }\n'
                f"mpath={shlex.quote(str(manifest))}\n"
                + loader
                + '\nprintf "mysbx-test: token-count=%d\\n" "$#"\n'
                '\nfor t in "$@"; do printf "mysbx-test: token=[%s]\\n" "$t"; done\n'
                '\necho "mysbx-test: loader boundary reached"\n'
            )
            return subprocess.run(
                [str(ROOTFS / "bin/busybox"), "sh", "-c", script, "sh",
                 "/bin/sh", "-c", "exit 7"],
                capture_output=True,
                text=True,
                env={"PATH": ""},
                timeout=20,
            )

        # The argv records ride AFTER env records; three tokens —
        # spaced, empty and `=`-carrying — plus a share record the
        # loop must still consume after them.
        records = [
            "env\tHOME\tL215c2J4LWhvbWU=\n",
            "argv\t0\t" + base64_of("/bin/sh") + "\n",
            "argv\t1\t" + base64_of("-c") + "\n",
            "argv\t2\t" + base64_of("exit 7") + "\n",
        ]
        result = run_manifest(records)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: token-count=3", result.stdout)
        self.assertIn("token=[/bin/sh]", result.stdout)
        self.assertIn("token=[-c]", result.stdout)
        self.assertIn("token=[exit 7]", result.stdout)
        # The positional split is NOT the spawned argv: the check
        # sh -c "exit 7" needs the tokens SPACED as one argument,
        # which the loader's set -- guarantees.

        # An EMPTY token survives the roundtrip.
        records = [
            "argv\t0\t" + base64_of("/bin/sh") + "\n",
            "argv\t1\t" + base64_of("") + "\n",
        ]
        result = run_manifest(records)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("mysbx-test: token-count=2", result.stdout)
        self.assertIn("token=[]", result.stdout)

        # A reordered or truncated index is a refusal, never a
        # silently reordered payload.
        result = run_manifest(["argv\t1\t" + base64_of("x") + "\n"])
        self.assertEqual(result.returncode, 125, result.stdout + result.stderr)
        self.assertIn("out of order", result.stderr)

        # A bad base64 token is a refusal.
        result = run_manifest(["argv\t0\tnot!base64\n"])
        self.assertEqual(result.returncode, 125, result.stdout + result.stderr)
        self.assertIn("cannot decode the manifest argv 0", result.stderr)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("rootfs", type=Path)
    parser.add_argument("--init", type=Path, help="override only the init text for a baseline comparison")
    args, remaining = parser.parse_known_args()
    ROOTFS = args.rootfs.resolve()
    INIT = (args.init or ROOTFS / "bin/mysbx-init").read_text()
    unittest.main(argv=[__file__, *remaining])
