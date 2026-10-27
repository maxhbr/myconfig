# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

"""Check the generated init without mounts or writes to the host store."""

import argparse
import base64
import os
import pty
from pathlib import Path
import shlex
import shutil
import sqlite3
import stat
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

    def run_reconcile(self, fail_query=None, store_disk=False, baked_db=None):
        # Extract the init's complete copy/reconcile block, not a SQL replica.
        # The block opens at the db_src pick (the store mode selects the
        # source db: the baked disk's own or the host share's) so the
        # extraction is self-contained; with the env unset the harness
        # exercises the HOST branch, the one with a live share to diverge
        # from the db snapshot. store_disk=True runs the BAKED branch:
        # the erofs's own /nix/var/nix/db maps to the baked_db fixture,
        # the reconcile gate is closed by the env marker.
        start = INIT.index(
            '    if [ "${MYSBX_KRUN_STORE_DISK:-}" = "1" ]; then\n'
            "        db_src=/nix/var/nix/db"
        )
        reconcile = INIT.index(
            '    if [ "${MYSBX_KRUN_STORE_DISK:-}" != "1" ]'
            " && [ -f /mysbx-nix/state/db/db.sqlite ]; then",
            start,
        )
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
        if store_disk:
            block = block.replace("/nix/var/nix/db", str(baked_db))
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
            env={"PATH": "", **({"MYSBX_KRUN_STORE_DISK": "1"} if store_disk else {})},
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

    def make_block_devs(self, *ro_flags):
        # A faked /sys/block: one vd* per flag value, each with the
        # ro file the scans cat. The init's /sys/block is absolute
        # in the guest, so the extraction substitutes this tree.
        sys_block = self.base / "sys/block"
        for n, ro in enumerate(ro_flags):
            dev = sys_block / ("vd" + chr(ord("a") + n))
            dev.mkdir(parents=True, exist_ok=True)
            (dev / "ro").write_text(str(ro))
        return sys_block

    def run_init_block(self, block, sys_block):
        script = (
            "set -eu\n"
            f"BB={shlex.quote(str(ROOTFS / 'bin/busybox'))}\n"
            'fail() { echo "mysbx-init: $*" >&2; exit 125; }\n'
            'step() { echo "mysbx-init: $*"; }\n'
            + block.replace("/sys/block", shlex.quote(str(sys_block)))
        )
        return subprocess.run(
            [str(ROOTFS / "bin/busybox"), "sh", "-c", script],
            capture_output=True,
            text=True,
            env={"PATH": ""},
            timeout=20,
        )

    def run_pick_vd(self, ro_flags, want, role):
        # The extracted pick_vd, EXECUTED against the faked /sys/block:
        # the single-match pick, the ambiguity refusal and the empty
        # case are behavior here, not strings pinned in the source.
        start = INIT.index("pick_vd() {")
        end = INIT.index("\n}\n", start) + len("\n}\n")
        call = "\npick_vd %s %s\n" % (shlex.quote(str(want)), shlex.quote(role))
        return self.run_init_block(INIT[start:end] + call, self.make_block_devs(*ro_flags))

    def run_unannounced_scan(self, ro_flags):
        # The else arm's refusal scan (a read-only vd* the env never
        # announced), likewise executed: with a read-only disk
        # present it must refuse, with writable disks only it passes.
        start = INIT.index(
            "for d in /sys/block/vd*; do",
            INIT.index("# manifest forgot"),
        )
        end = INIT.index("\n    done\n", start) + len("\n    done\n")
        return self.run_init_block(INIT[start:end], self.make_block_devs(*ro_flags))

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
            '-o lowerdir=/tmp/mysbx-shares/stage-ro/store,upperdir=/mysbx-nix/store-upper',
            'exec "$@"',
        ]
        offsets = [INIT.index(phase) for phase in phases]
        self.assertEqual(offsets, sorted(offsets), "DB validation must precede /nix shadowing and the overlay")
        # Both store modes mount exactly one store overlay (the baked
        # erofs lower and the host-share lower), and only the SHARE mode
        # needs the tmpfs mountpoint trick — /nix/store is a baked symlink
        # there, a real erofs directory in the baked mode.
        self.assertEqual(INIT.count('"$BB" mount -t overlay overlay'), 2)
        self.assertEqual(
            INIT.count('-o lowerdir=/nix/store,upperdir=/mysbx-nix/store-upper'), 1
        )
        self.assertEqual(
            INIT.count(
                '-o lowerdir=/tmp/mysbx-shares/stage-ro/store,'
                "upperdir=/mysbx-nix/store-upper"
            ),
            1,
        )
        self.assertEqual(INIT.count('"$BB" mount -t tmpfs tmpfs /nix \\\n'), 1)
        self.assertEqual(
            os.readlink(ROOTFS / "nix/store"),
            "/tmp/mysbx-shares/stage-ro/store",
        )

    def test_store_disk_section(self):
        # docs/design/guest-store.md: the guest-own store disk. The erofs
        # mounts BEFORE the scratch section (the db copy reads /nix/var/nix
        # off it), read-only at the device level; both disks identify
        # through the one shared pick_vd scan, by their /sys/block/*/ro
        # flag — never by name.
        erofs = INIT.index('"$BB" mount -t erofs -o ro "$store_dev" /nix')
        announce = INIT.index('if [ "${MYSBX_KRUN_STORE_DISK:-}" = "1" ]; then')
        scratch = INIT.index('if [ "${MYSBX_KRUN_SCRATCH:-}" = "1" ]; then')
        db_copy = INIT.index('db_src=/nix/var/nix/db')
        baked_overlay = INIT.index('-o lowerdir=/nix/store,upperdir=/mysbx-nix/store-upper')
        self.assertLess(announce, erofs)
        self.assertLess(erofs, scratch)
        self.assertLess(scratch, db_copy)
        self.assertLess(db_copy, baked_overlay)
        # The two picks route through the one shared scan — the ro
        # flag 1 picks the store disk, 0 the scratch (the executed
        # behavior, both picks and refusals, is the pick_vd tests
        # below; these asserts only pin the wiring of the calls).
        self.assertIn('store_dev=$(pick_vd 1 "the store disk")', INIT)
        self.assertIn('scratch_dev=$(pick_vd 0 "the scratch")', INIT)

    def test_pick_vd_finds_the_single_matching_disk(self):
        # One disk per mode attached: the store pick finds the
        # read-only vd*, the scratch pick the writable one, and a
        # pick whose mode has no disk comes back empty (the caller
        # refuses that with its own message).
        result = self.run_pick_vd((1, 0), 1, "the store disk")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "/dev/vda")
        result = self.run_pick_vd((1, 0), 0, "the scratch")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "/dev/vdb")
        result = self.run_pick_vd((0,), 1, "the store disk")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "")

    def test_pick_vd_refuses_an_ambiguous_attach(self):
        # Two disks of one mode are an ambiguous attach the pick
        # refuses — never a silent last-one-wins mount.
        result = self.run_pick_vd((1, 1, 0), 1, "the store disk")
        self.assert_failed(
            result,
            "2 read-only /dev/vd* devices — the store disk is ambiguous, refusing to guess",
        )
        result = self.run_pick_vd((0, 0, 1), 0, "the scratch")
        self.assert_failed(
            result,
            "2 writable /dev/vd* devices — the scratch is ambiguous, refusing to guess",
        )

    def test_unannounced_read_only_disk_refuses(self):
        # The refusal side of the announce-or-refuse rule: a
        # read-only vd* without the env marker is a disk the
        # manifest forgot — never a silent store the payload could
        # see unmounted. Writable disks only: nothing to refuse.
        result = self.run_unannounced_scan((1, 0))
        self.assert_failed(result, "exists but MYSBX_KRUN_STORE_DISK is unset")
        result = self.run_unannounced_scan((0,))
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_trace_phases(self):
        # The traced step names of the db reconcile: the split of the
        # old single "validating" step into enumerate + check (each
        # with its own timing) must keep the ORDER and the completion
        # marker the live validation greps for. The overlay phase anchors
        # the SHARE branch's mount (the baked branch's overlay mounts on
        # the erofs /nix/store directly, earlier in the file).
        phases = [
            'step "enumerating the registered store paths from the copied nix db"',
            "SELECT path FROM ValidPaths WHERE path LIKE",
            'fail "foreign-key violations remain in the copied nix database"',
            'step "copied nix db validation complete"',
            'step "mounting the store overlay (fresh scratch upper)"',
            '-o lowerdir=/tmp/mysbx-shares/stage-ro/store,upperdir=/mysbx-nix/store-upper',
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

    def test_baked_mode_copies_the_disk_own_db(self):
        # The baked branch of the db copy (docs/design/guest-store.md):
        # db_src is the erofs's OWN /nix/var/nix/db, and the copy loop
        # must follow $db_src. The host-share fixture stays EMPTY —
        # the baked guest has no share mounted at that path, and a
        # seeded fixture would let a loop that ignores $db_src pass:
        # here such a loop copies nothing and the db.sqlite assert
        # fails. big-lock/reserved are skipped in both modes; the
        # reconcile gate is closed by the env marker, so the db
        # arrives exactly as baked.
        baked = self.base / "baked-db"
        baked.mkdir()
        with sqlite3.connect(baked / "db.sqlite") as db:
            db.executescript(SCHEMA)
            db.execute("INSERT INTO ValidPaths VALUES (1, '/nix/store/aaaa-valid-path')")
        (baked / "big-lock").write_text("lock")
        (baked / "reserved").write_text("space")
        result = self.run_reconcile(store_disk=True, baked_db=baked)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("copying the nix db", result.stdout)
        self.assertNotIn("no nix-var db", result.stdout)
        copied = self.scratch / "state/db"
        self.assertTrue(
            (copied / "db.sqlite").is_file(),
            "the baked db never reached the scratch",
        )
        self.assertFalse((copied / "big-lock").exists())
        self.assertFalse((copied / "reserved").exists())
        with sqlite3.connect(copied / "db.sqlite") as db:
            self.assertEqual(
                db.execute("SELECT count(*) FROM ValidPaths").fetchone()[0], 1
            )
        self.assertNotIn("enumerating the registered store paths", result.stdout)

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
        # The resolver rewrite (bd myconfig-m0to): the host's own
        # nameservers are host-local — behind the SLiRP NAT they
        # point into the GUEST, where nothing listens. The step
        # rewrites /etc/resolv.conf to SLiRP's DNS proxy (.3, not
        # the .2 gateway) and keeps the localhost hosts mapping.
        self.assertIn("nameserver 10.0.2.3", block)
        self.assertIn("> /etc/resolv.conf", block)
        self.assertIn("127.0.0.1 localhost", block)
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
        # The mknod sub-check is capability-gated: an unprivileged
        # nix builder lacks CAP_MKNOD, so there the DESIGNED
        # fallback is asserted instead (bd myconfig-opx8).
        start = INIT.index("    _exit_port=/dev/virtio-ports/$exit_channel")
        end = INIT.index("    set +e", start)
        block = INIT[start:end]
        self.assertIn('/sys/class/virtio-ports', block)
        self.assertIn('mknod', block)

        try:
            os.mknod(self.base / "mknod-probe", 0o600 | stat.S_IFCHR,
                     os.makedev(1, 3))
            have_mknod = True
            (self.base / "mknod-probe").unlink()
        except OSError:
            have_mknod = False

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
        if have_mknod:
            # The node did NOT exist in the fixture /dev: the mknod
            # fallback must have created it from the dev file.
            self.assertEqual(result.stdout.strip(),
                             f"mysbx-test: port={dev}/vport0p2",
                             result.stdout + result.stderr)
            self.assertTrue((dev / "vport0p2").exists())
        else:
            # No CAP_MKNOD (the nix build sandbox): the designed
            # degradation — the port stays the udev path and no
            # node appears.
            self.assertEqual(result.stdout.strip(),
                             f"mysbx-test: port={dev}/virtio-ports/mysbx-exit",
                             result.stdout + result.stderr)
            self.assertFalse((dev / "vport0p2").exists())

        # With the node already present (the devtmpfs shape on a
        # real guest), the existing node is used. A regular file
        # satisfies the init's [ -e ] probe just as well, so this
        # covers the primary real-guest branch even without
        # CAP_MKNOD.
        (dev / "vport0p2").unlink(missing_ok=True)
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
        # The launch shape (bd myconfig-w8jd): setsid -c acquires the
        # console as ctty for the payload — synchronous, never '&'.
        self.assertIn('"$BB" setsid -c "$@"\n    _exit_code=$?', branch)

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

    def _ctty_probe(self, master, slave, launch):
        # A session-leader waiter drives the launch line VERBATIM
        # (via `set --`) over the console, exactly like the init.
        # The payload must OPEN /dev/tty: existence pins nothing
        # (/dev/tty exists in devtmpfs even with no ctty, so
        # `test -e` passes either way), and a bare `exec 3</dev/tty`
        # would succeed into a *new* session were the shell ever a
        # leader — so the open happens inside a subshell, which can
        # never acquire a ctty on its own. The outcome maps into
        # the exit code: 5 = ctty acquired, 9 = open failed (ENXIO).
        bb = shlex.quote(str(ROOTFS / "bin/busybox"))
        script = (
            "set -eu\n"
            f"BB={bb}\n"
            'set -- "$BB" sh -c '
            + shlex.quote("(exec 3</dev/tty) && exit 5; exit 9")
            + "\n"
            "set +e\n"
            f"{launch}\n"
            "code=$?\n"
            "set -e\n"
            'exit "$code"\n'
        )
        proc = subprocess.Popen(
            [str(ROOTFS / "bin/busybox"), "sh"],
            stdin=slave, stdout=slave, stderr=slave,
            start_new_session=True,  # the PID-1 shape: session leader
            env={"PATH": ""},
        )
        # Feed the script on the console stdin.
        os.write(master, script.encode())
        try:
            return proc.wait(timeout=20)
        finally:
            if proc.poll() is None:
                proc.kill()

    def test_the_exit_channel_payload_acquires_the_console_as_ctty(self):
        # bd myconfig-w8jd: PID 1 owns the session, so a plain child
        # never gets a controlling terminal — opening /dev/tty is
        # ENXIO, no job control, ^C reaches nobody. The launch wraps
        # the payload in `setsid -c`: a new session leader with the
        # console as ctty, exec'd in place so $? still carries the
        # payload's code. Live-probed on a pty in the EXACT PID-1
        # shape: the session-leader waiter forks the launch as a
        # non-leader (exec in place), the child must OPEN /dev/tty
        # and its code (5 = ctty, 9 = not) must reach the waiter.
        try:
            master, slave = pty.openpty()
        except OSError:
            self.skipTest("no pty in this environment")
        try:
            self.assertTrue('"$BB" setsid -c "$@"' in INIT,
                            "the ctty launch line is gone from the init")
            rc = self._ctty_probe(master, slave, '"$BB" setsid -c "$@"')
            # rc==5 proves BOTH halves at once — the /dev/tty OPEN
            # succeeded inside setsid -c (the ctty) AND the code
            # propagated through setsid to the session-leader waiter.
            # rc==9: no ctty (the regression this test exists for);
            # rc==0: the payload never ran; 127/126: the exec failed.
            self.assertEqual(rc, 5,
                             "the ctty payload open failed (exit "
                             f"{rc}) or the code was lost")

            # NEGATIVE CONTROL (review r4/3): the same probe against
            # a launch with setsid STRIPPED — the exact regression
            # shape — must yield the failure code, proving this
            # test can fail at all. Without it, a vacuous probe
            # (existence check, unconditional exit) passes both.
            broken = INIT.replace('"$BB" setsid -c "$@"', '"$@"')
            self.assertIsNot(broken, INIT,
                             "the setsid launch line to strip is gone")
            rc = self._ctty_probe(master, slave, '"$@"')
            self.assertEqual(rc, 9,
                             "the stripped-setsid launch must fail the "
                             f"ctty open (exit {rc} instead of 9)")
        finally:
            os.close(master)
            os.close(slave)

    def test_qemu_pid1_bootstrap_mounts_the_pty_shm_fd_and_lo_surface(self):
        # bd myconfig-af0j: the qemu PID-1 arm must complete the
        # guest bootstrap libkrun's implicit init does for the krun
        # guest — devpts (mux sessions allocate PTYs), a tmpfs
        # /dev/shm, the /dev/fd link and lo UP. Every step lives
        # INSIDE the /proc/uptime guard so the krun arm stays
        # byte-identical. The mounts themselves need a real PID-1
        # guest (bd myconfig-26z4's live probes); here the text and
        # the guard placement are pinned.
        start = INIT.index("if [ ! -f /proc/uptime ]; then")
        end = INIT.index("fi", start)
        guard = INIT[start:end]
        for needle in (
            "mount -t devpts devpts /dev/pts",
            "mount -t tmpfs tmpfs /dev/shm",
            "ln -s /proc/self/fd /dev/fd",
            "ip link set lo up",
            "mkdir -p /dev/pts /dev/shm",
        ):
            self.assertIn(needle, guard,
                          f"the qemu PID-1 bootstrap is missing: {needle}")
        # All five steps must sit BEFORE the guard's fi — the krun
        # arm (a mounted /proc skips the block) is unchanged.
        self.assertLess(guard.index("ip link set lo up"), len(guard))
        # The krun path must NOT bring up lo or mount devpts itself:
        # nothing outside the guard may carry the steps.
        after = INIT[end:]
        self.assertNotIn("mount -t devpts", after)
        self.assertNotIn("ip link set lo up", after)

    def test_console_size_marker_applies_stty_to_the_console(self):
        # bd myconfig-a2uq: the qemu guest's serial console carries no
        # winsize — the init must stty the console from the manifest's
        # MYSBX_MICROVM_CONSOLE_SIZE marker before the payload runs, and
        # the step must be GATED (the krun guest and piped runs set no
        # marker, and a malformed marker must never kill the init).
        start = INIT.index('_csize="')
        block = INIT[start:INIT.index('step "exec-ing the payload', start)]
        for needle in (
            'MYSBX_MICROVM_CONSOLE_SIZE',
            'stty rows',
        ):
            self.assertIn(needle, block,
                          f"the console-size step is missing: {needle}")
        # A malformed marker skips the stty instead of failing the init
        # (the 2>/dev/null || : on the stty line and the case arms),
        # and the stty targets the console fd 0 by default.
        self.assertIn('|| :', block,
                      "the stty must not be fatal on a refused console")
        self.assertNotIn("fail \"", block,
                        "a bad marker must be ignored, never fatal")
        # The step sits before the payload exec (the console must carry
        # the size before the payload can observe it).
        self.assertLess(INIT.index('_csize="'),
                        INIT.index('step "exec-ing the payload'))

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

        # A token ENDING in a newline survives byte-identically (bd
        # myconfig-m6r1): $( ) strips ALL trailing newlines, so the
        # loader decodes with a sentinel — the six-byte `hello\n`
        # must arrive as six bytes, and an ALL-newline token must
        # not collapse to the empty string. The harness prints
        # token=[%s] with a literal newline inside, so the expected
        # output carries the line break INSIDE the brackets.
        records = [
            "argv\t0\t" + base64_of("hello\n") + "\n",
            "argv\t1\t" + base64_of("\n") + "\n",
        ]
        result = run_manifest(records)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("token-count=2", result.stdout)
        self.assertIn("token=[hello\n]", result.stdout)
        self.assertIn("token=[\n]", result.stdout)

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
