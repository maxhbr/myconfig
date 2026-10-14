#!/usr/bin/env python3
"""Run a controlled shared-tree walk and measure the host launcher's fd count."""

import argparse
import base64
from collections import Counter
import io
import math
import os
from pathlib import Path
import resource
import select
import shlex
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import time
import tomllib
import unittest
from unittest import mock

NOFILE = 65536
STORE_ROOTS = ("/nix/store", "/mysbx-krun-stage/ro/store")
WATCHDOG_ENV = "MYSBX_KRUN_INODE_WATCHDOG"
WATCHDOG_INTERVAL_ENV = "MYSBX_KRUN_INODE_WATCHDOG_INTERVAL"
GUEST = r"""
import os
import sys

def fail(error):
    raise error

root, rounds, expected = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
print(f"guest: kernel={os.uname().release}; starting {rounds} walks of {expected} files", flush=True)
for round in range(rounds):
    count = 0
    for directory, _, files in os.walk(root, onerror=fail):
        for name in files:
            with open(os.path.join(directory, name), "rb") as source:
                if source.read() != b"probe\n":
                    raise RuntimeError("unexpected fixture content")
            count += 1
    if count != expected:
        raise RuntimeError(f"walk returned {count} files, expected {expected}")
    print(f"round {round + 1}: read {count} files", flush=True)
"""

# The watchdog-validation payload (bd myconfig-dj4): a store-share
# walk pins one guest inode — and through the FUSE lookup ref, one
# host launcher fd — per visited entry, with nothing to evict it in
# an unpressured VM (the live wedge). The payload walks the SHARED
# store like a real agent session (greps/evals over /nix/store),
# samples the guest's allocated inodes during each walk, and waits for
# reclamation if none was observed: every round must show a drop,
# proving the guest-side drop (drop_caches=2) bounds
# the accumulation instead of growing monotonically toward EMFILE.
GUEST_WATCHDOG = r"""
import os
import sys
import time

root = sys.argv[1]
rounds = int(sys.argv[2])
threshold = int(sys.argv[3])
interval = float(sys.argv[4])


def inodes():
    with open("/proc/sys/fs/inode-state") as source:
        return int(source.read().split()[0])


print(f"guest: kernel={os.uname().release}; store scan of {root}", flush=True)
for round in range(rounds):
    count = 0
    peak = inodes()
    reclaimed = None
    for directory, _, files in os.walk(root, onerror=lambda error: None):
        count += 1 + len(files)
        after = inodes()
        peak = max(peak, after)
        if peak > threshold and after < peak * 0.9:
            reclaimed = (peak, after)
    print(f"round {round + 1}: scanned {count} entries, peak inodes={peak}", flush=True)
    if peak <= threshold:
        print(f"WARN: the walk never exceeded the watchdog threshold "
              f"({peak} <= {threshold}); the eviction path is untested", flush=True)
    # Only wait if no drop was already observed during traversal.
    deadline = time.monotonic() + 4 * interval + 10
    while reclaimed is None:
        after = inodes()
        peak = max(peak, after)
        if peak > threshold and after < peak * 0.9:
            reclaimed = (peak, after)
        elif time.monotonic() >= deadline:
            raise RuntimeError(
                f"the watchdog did not reclaim: inodes {peak} -> {after} "
                f"after {4 * interval + 10:.0f}s")
        else:
            time.sleep(0.5)
    print(f"round {round + 1}: watchdog reclaimed "
          f"({reclaimed[0]} -> {reclaimed[1]})", flush=True)
print("guest: store scan complete", flush=True)
"""


def guest_argv(root, rounds, files):
    # Libkrun transports argv through a printable-ASCII, double-quoted command line.
    encoded = base64.b64encode(GUEST.encode("utf-8")).decode("ascii")
    code = f"exec(__import__('base64').b64decode('{encoded}'))"
    return ["python3", "-c", code, str(root), str(rounds), str(files)]


def is_launcher(command):
    args = command.split(b"\0")
    name = args[0].rsplit(b"/", 1)[-1]
    return name in (b"mysbx-krun", b".mysbx-krun-wrapped") and b"--rootfs" in args


def find_launcher(pid, proc_root=Path("/proc")):
    pending, seen = [pid], set()
    while pending:
        current = pending.pop()
        if current in seen:
            continue
        seen.add(current)
        try:
            if is_launcher((proc_root / str(current) / "cmdline").read_bytes()):
                return current
            children = proc_root / str(current) / "task" / str(current) / "children"
            pending.extend(map(int, children.read_text().split()))
        except (FileNotFoundError, ProcessLookupError):
            continue
    return None


def read_limits(pid, proc_root=Path("/proc")):
    for line in (proc_root / str(pid) / "limits").read_text().splitlines():
        if line.startswith("Max open files"):
            return tuple(line.split()[3:5])
    raise RuntimeError("launcher limits did not include Max open files")


def child_limits():
    resource.setrlimit(resource.RLIMIT_NOFILE, (NOFILE, NOFILE))


def stop(proc, launcher_fd=None):
    if launcher_fd is not None:
        try:
            signal.pidfd_send_signal(launcher_fd, signal.SIGTERM)
        except ProcessLookupError:
            pass
    if proc.poll() is None:
        try:
            os.killpg(proc.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            if launcher_fd is not None:
                try:
                    signal.pidfd_send_signal(launcher_fd, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            proc.wait()
    if launcher_fd is not None and not select.select([launcher_fd], [], [], 5)[0]:
        try:
            signal.pidfd_send_signal(launcher_fd, signal.SIGKILL)
        except ProcessLookupError:
            pass
        if not select.select([launcher_fd], [], [], 5)[0]:
            raise RuntimeError("the test launcher did not stop")


def fd_scope(target, data):
    if f"/{data.parent.name}/" in target:
        return "fixture"
    if any(target == root or target.startswith(root + "/") for root in STORE_ROOTS):
        return "store"
    return "other"


def fd_snapshot(pid, data, proc_root=Path("/proc")):
    process = proc_root / str(pid)
    names = list((process / "fd").iterdir())
    counts, examples = Counter(), {}
    for descriptor in names:
        try:
            target = os.readlink(descriptor)
            mode = descriptor.stat().st_mode
            info = (process / "fdinfo" / descriptor.name).read_text()
            flags = next(int(line.split()[1], 8) for line in info.splitlines()
                         if line.startswith("flags:"))
        except (OSError, ValueError, StopIteration):
            counts[("unreadable or gone", "unknown", "unknown")] += 1
            continue
        scope = fd_scope(target, data)
        kind = "directory" if stat.S_ISDIR(mode) else (
            "file" if stat.S_ISREG(mode) else "other"
        )
        access = "O_PATH" if flags & os.O_PATH else "handle"
        key = (scope, kind, access)
        counts[key] += 1
        examples.setdefault(key, target)
    lines = [f"fd snapshot: pid={pid}; listed={len(names)} (live, non-atomic)"]
    for key, count in sorted(counts.items()):
        lines.append(f"  {' '.join(key)}: {count}; example={examples.get(key, '-')!r}")
    return "\n".join(lines)


def guest_watchdog_argv(root, rounds, threshold, interval):
    encoded = base64.b64encode(GUEST_WATCHDOG.encode("utf-8")).decode("ascii")
    code = f"exec(__import__('base64').b64decode('{encoded}'))"
    return ["python3", "-c", code, str(root), str(rounds), str(threshold), str(interval)]


def run_store_scan(args, repo):
    # The watchdog validation (bd myconfig-dj4): the guest walks the
    # SHARED store — the workload of every live wedge, unlike the
    # private fixture (whose inodes the guest evicts on its own) —
    # while the host side samples the launcher's fd count. The
    # watchdog is armed through a scratch repo's [env] (the normal
    # config route: [env] merges into the payload env, the launcher
    # carries it in the manifest, mysbx-init reads it), with a LOW
    # threshold and a SHORT interval so the drops are observable
    # inside the walk. PASS needs BOTH halves: the guest reports the
    # watchdog reclaiming each round, AND the host launcher's fd
    # peak stays under the budget (roughly one pinned fd per guest
    # inode, so the threshold must sit below the fd budget).
    if args.watchdog_threshold >= args.fd_budget:
        raise RuntimeError(
            f"--watchdog-threshold ({args.watchdog_threshold}) must stay "
            f"below --fd-budget ({args.fd_budget})"
        )
    scratch = Path(tempfile.mkdtemp(prefix=".mysbx-watchdog-", dir=repo))
    try:
        return _store_scan_once(args, repo, scratch)
    finally:
        shutil.rmtree(scratch.with_name(scratch.name + ".mysbx"), ignore_errors=True)
        shutil.rmtree(scratch, ignore_errors=True)


def _store_scan_once(args, repo, scratch):
    scratch_config = scratch.with_name(scratch.name + ".mysbx")
    scratch_config.mkdir(exist_ok=True)
    (scratch_config / "config.toml").write_text(
        'backend = "krun"\n'
        "[env]\n"
        f'{WATCHDOG_ENV} = "{args.watchdog_threshold}"\n'
        f'{WATCHDOG_INTERVAL_ENV} = "2"\n'
    )
    log_fd, log_name = tempfile.mkstemp(prefix="mysbx-krun-watchdog-", suffix=".log")
    print(
        f"Watchdog validation: store scan; threshold={args.watchdog_threshold}, "
        f"launcher fd budget: {args.fd_budget}",
        flush=True,
    )
    print(f"Log: {log_name}", flush=True)
    with os.fdopen(log_fd, "w") as log:
        proc = subprocess.Popen(
            [
                args.mysbx,
                "run",
                "--",
                *guest_watchdog_argv(
                    "/nix/store", args.rounds, args.watchdog_threshold, 2
                ),
            ],
            cwd=scratch,
            stdin=subprocess.DEVNULL,
            stdout=log,
            stderr=subprocess.STDOUT,
            start_new_session=True,
            preexec_fn=child_limits,
        )
        peak, launcher, launcher_fd = 0, None, None
        deadline = time.monotonic() + args.timeout
        try:
            while proc.poll() is None:
                if time.monotonic() >= deadline:
                    raise RuntimeError("the krun store scan timed out")
                if launcher is None:
                    found = find_launcher(proc.pid)
                    if found is not None:
                        try:
                            candidate_fd = os.pidfd_open(found)
                            try:
                                if (
                                    find_launcher(proc.pid) == found
                                    and not select.select([candidate_fd], [], [], 0)[0]
                                ):
                                    launcher_fd, launcher = candidate_fd, found
                            finally:
                                if launcher_fd != candidate_fd:
                                    os.close(candidate_fd)
                        except ProcessLookupError:
                            pass
                if launcher is not None:
                    try:
                        peak = max(peak, len(os.listdir(f"/proc/{launcher}/fd")))
                    except (FileNotFoundError, ProcessLookupError):
                        pass
                    if peak > args.fd_budget:
                        snapshot = fd_snapshot(launcher, scratch)
                        print(snapshot, file=sys.stderr, flush=True)
                        print(snapshot, file=log, flush=True)
                        raise RuntimeError(
                            f"launcher fd count {peak} exceeded budget {args.fd_budget}"
                        )
                time.sleep(0.1)
            if proc.returncode != 0:
                raise RuntimeError(f"krun exited {proc.returncode}; read {log_name}")
            log.flush()
            with open(log_name) as check:
                reclaimed = [line for line in check if "watchdog reclaimed" in line]
            if not reclaimed:
                raise RuntimeError(
                    f"no watchdog reclaim is reported in {log_name}; "
                    "the guest-side half of the fix did not trigger"
                )
            if launcher is None or peak == 0:
                raise RuntimeError("no launcher fd sample was collected")
        finally:
            try:
                stop(proc, launcher_fd)
            finally:
                if launcher_fd is not None:
                    os.close(launcher_fd)
        print(
            f"PASS: watchdog store scan; launcher peak={peak}, "
            f"limit={NOFILE}, budget={args.fd_budget}; "
            f"guest reclaims: {len(reclaimed)}"
        )
    return 0


def run(args):
    if not os.access("/dev/kvm", os.R_OK | os.W_OK):
        print(
            "SKIP: /dev/kvm must be readable and writable on the test host",
            file=sys.stderr,
        )
        return 77
    repo = args.repo.resolve(strict=True)
    if not repo.is_dir():
        raise RuntimeError("the repository path must name a directory")
    hard_limit = resource.getrlimit(resource.RLIMIT_NOFILE)[1]
    if hard_limit != resource.RLIM_INFINITY and hard_limit < NOFILE:
        raise RuntimeError(f"the host hard fd limit must be at least {NOFILE}")
    if args.store_scan:
        return run_store_scan(args, repo)
    with tempfile.TemporaryDirectory(prefix=".mysbx-fd-stress-", dir=repo) as temporary:
        data = Path(temporary) / "data"
        data.mkdir()
        for index in range(args.files):
            directory = data / str(index // 100)
            if index % 100 == 0:
                directory.mkdir()
            (directory / str(index)).write_bytes(b"probe\n")
        log_fd, log_name = tempfile.mkstemp(prefix="mysbx-krun-fd-", suffix=".log")
        print(
            f"Fixture: {args.files} files; launcher fd budget: {args.fd_budget}",
            flush=True,
        )
        print(f"Log: {log_name}", flush=True)
        with os.fdopen(log_fd, "w") as log:
            proc = subprocess.Popen(
                [
                    args.mysbx, "run", "--backend", "krun", "--",
                    *guest_argv(data.relative_to(repo), args.rounds, args.files),
                ],
                cwd=repo,
                stdin=subprocess.DEVNULL,
                stdout=log,
                stderr=subprocess.STDOUT,
                start_new_session=True,
                preexec_fn=child_limits,
            )
            peak, launcher, launcher_fd = 0, None, None
            deadline = time.monotonic() + args.timeout
            try:
                while proc.poll() is None:
                    if time.monotonic() >= deadline:
                        raise RuntimeError("the krun walk timed out")
                    if launcher is None:
                        found = find_launcher(proc.pid)
                        if found is not None:
                            try:
                                candidate_fd = os.pidfd_open(found)
                                try:
                                    # Attach only to a live process still in this test's tree.
                                    if (
                                        find_launcher(proc.pid) == found
                                        and not select.select([candidate_fd], [], [], 0)[0]
                                    ):
                                        launcher_fd, launcher = candidate_fd, found
                                finally:
                                    if launcher_fd != candidate_fd:
                                        os.close(candidate_fd)
                            except ProcessLookupError:
                                pass
                    if launcher is not None:
                        try:
                            limits = read_limits(launcher)
                            if limits != (str(NOFILE), str(NOFILE)):
                                raise RuntimeError(
                                    f"launcher fd limits were {limits}, expected {NOFILE}"
                                )
                            peak = max(peak, len(os.listdir(f"/proc/{launcher}/fd")))
                        except (FileNotFoundError, ProcessLookupError):
                            pass
                        if peak > args.fd_budget:
                            try:
                                snapshot = fd_snapshot(launcher, data)
                            except OSError as error:
                                snapshot = f"fd snapshot unavailable: {error}"
                            print(snapshot, file=sys.stderr, flush=True)
                            print(snapshot, file=log, flush=True)
                            raise RuntimeError(
                                f"launcher fd count {peak} exceeded budget {args.fd_budget}"
                            )
                    time.sleep(0.1)
                if proc.returncode != 0:
                    raise RuntimeError(f"krun exited {proc.returncode}; read {log_name}")
                if launcher is None or peak == 0:
                    raise RuntimeError("no launcher fd sample was collected")
            finally:
                try:
                    stop(proc, launcher_fd)
                finally:
                    if launcher_fd is not None:
                        os.close(launcher_fd)
        print(
            f"PASS: {args.rounds} walks; launcher peak={peak}, "
            f"limit={NOFILE}, budget={args.fd_budget}"
        )
    return 0


class MonitorTests(unittest.TestCase):
    def test_guest_commandline(self):
        argv = guest_argv(Path(".mysbx-fd-stress-test/data"), 3, 100000)
        # Match libkrun's collapse_str_array and kernel::cmdline constraints.
        serialized = " ".join(f'"{arg}"' for arg in argv)
        self.assertTrue(all(" " <= char <= "~" for char in serialized))
        self.assertNotIn('"', argv[2])
        self.assertEqual(shlex.split(serialized), argv)

    def test_watchdog_guest_commandline(self):
        argv = guest_watchdog_argv("/nix/store", 2, 20000, 2)
        serialized = " ".join(f'"{arg}"' for arg in argv)
        self.assertTrue(all(" " <= char <= "~" for char in serialized))
        self.assertEqual(shlex.split(serialized), argv)
        # the payload must read the guest's own inode state and wait
        # out the watchdog, not just walk: decode the way the guest
        # does and inspect the source
        source = base64.b64decode(
            argv[2].removeprefix("exec(__import__('base64').b64decode('").removesuffix("')")
        )
        self.assertIn(b"/proc/sys/fs/inode-state", source)
        self.assertIn(b"watchdog reclaimed", source)

    def test_watchdog_env_names(self):
        self.assertEqual(WATCHDOG_ENV, "MYSBX_KRUN_INODE_WATCHDOG")
        self.assertEqual(
            WATCHDOG_INTERVAL_ENV, "MYSBX_KRUN_INODE_WATCHDOG_INTERVAL"
        )

    def test_watchdog_reclamation_timing(self):
        for name, rounds, values, reclaims, succeeds in [
            ("during walk", 1, [10000, 30000, 10000], 1, True),
            ("after walk", 1, [10000, 25000, 30000, 30000, 10000], 1, True),
            ("each round", 2, [10000, 30000, 10000, 10000, 31000, 10000], 2, True),
            ("no drop", 1, [10000, 30000, 30000], 0, False),
            ("small drop", 1, [10000, 30000, 28000], 0, False),
            ("below threshold", 1, [10000, 15000, 10000], 0, False),
            (
                "second round fails", 2,
                [10000, 30000, 10000, 10000, 30000, 30000], 1, False,
            ),
        ]:
            with self.subTest(name=name):
                samples = iter(values)
                output = io.StringIO()

                def inode_state(path):
                    self.assertEqual(path, "/proc/sys/fs/inode-state")
                    return io.StringIO(f"{next(samples, values[-1])} 0 0\n")

                with (
                    mock.patch.object(
                        sys, "argv", ["guest", "/nix/store", str(rounds), "20000", "2"],
                    ),
                    mock.patch("os.walk", return_value=[
                        ("/nix/store", [], ["a"]), ("/nix/store/a", [], ["b"]),
                    ]),
                    mock.patch("builtins.open", side_effect=inode_state),
                    mock.patch("time.monotonic", side_effect=iter(range(100))),
                    mock.patch("time.sleep") as sleep,
                    mock.patch.object(sys, "stdout", output),
                ):
                    if succeeds:
                        exec(GUEST_WATCHDOG, {})
                    else:
                        with self.assertRaisesRegex(
                            RuntimeError, "watchdog did not reclaim",
                        ):
                            exec(GUEST_WATCHDOG, {})
                    if name in ("during walk", "each round"):
                        sleep.assert_not_called()
                self.assertEqual(
                    output.getvalue().count("watchdog reclaimed"), reclaims,
                )

    def test_fd_budget_defaults_and_overrides(self):
        for flags, budget in [
            ([], 8192),
            (["--store-scan"], 40000),
            (["--fd-budget", "12345"], 12345),
            (["--store-scan", "--fd-budget", "8192"], 8192),
            (["--fd-budget", "30000", "--store-scan"], 30000),
        ]:
            with self.subTest(flags=flags):
                run_mock = mock.Mock(return_value=0)
                with mock.patch.object(sys, "argv", ["probe", *flags]), \
                        mock.patch.dict(globals(), run=run_mock):
                    self.assertEqual(main(), 0)
                self.assertEqual(run_mock.call_args.args[0].fd_budget, budget)

    def test_store_scan_rejects_threshold_at_budget(self):
        args = argparse.Namespace(watchdog_threshold=20000, fd_budget=20000)
        with self.assertRaisesRegex(RuntimeError, "must stay below --fd-budget"):
            run_store_scan(args, Path("/unused"))

    def test_store_scan_config_and_failure_cleanup(self):
        args = argparse.Namespace(
            mysbx="mysbx", watchdog_threshold=20000, fd_budget=40000, rounds=2,
        )
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary) / "repo"
            repo.mkdir()
            log_fd, log_name = tempfile.mkstemp(dir=temporary)

            def launch(argv, **kwargs):
                scratch = kwargs["cwd"]
                sidecar = scratch.with_name(scratch.name + ".mysbx")
                self.assertEqual(sidecar.parent, repo)
                self.assertFalse((scratch / ".mysbx").exists())
                config = tomllib.loads((sidecar / "config.toml").read_text())
                self.assertEqual(config, {
                    "backend": "krun",
                    "env": {WATCHDOG_ENV: "20000", WATCHDOG_INTERVAL_ENV: "2"},
                })
                raise OSError("launch failed")

            with mock.patch("tempfile.mkstemp", return_value=(log_fd, log_name)), \
                    mock.patch("subprocess.Popen", side_effect=launch) as popen:
                with self.assertRaisesRegex(OSError, "launch failed"):
                    run_store_scan(args, repo)
                popen.assert_called_once()
            self.assertEqual(list(repo.iterdir()), [])

    def test_store_scan_success_cleanup(self):
        args = argparse.Namespace(watchdog_threshold=20000, fd_budget=40000)
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary)

            def scan(args, repo, scratch):
                sidecar = scratch.with_name(scratch.name + ".mysbx")
                sidecar.mkdir()
                (sidecar / "config.toml").write_text('backend = "krun"\n')
                return 0

            with mock.patch.dict(
                globals(), _store_scan_once=mock.Mock(side_effect=scan),
            ):
                self.assertEqual(run_store_scan(args, repo), 0)
            self.assertEqual(list(repo.iterdir()), [])

    def test_guest_walks_and_content_check(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            data = root / "data"
            data.mkdir()
            fixture = data / "file"
            fixture.write_bytes(b"probe\n")
            argv = guest_argv(Path("data"), 3, 1)
            argv[0] = sys.executable
            result = subprocess.run(
                argv, cwd=root, capture_output=True, text=True, timeout=10,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(
                result.stdout,
                f"guest: kernel={os.uname().release}; starting 3 walks of 1 files\n"
                "round 1: read 1 files\nround 2: read 1 files\nround 3: read 1 files\n",
            )
            fixture.write_bytes(b"corrupt")
            result = subprocess.run(
                argv, cwd=root, capture_output=True, text=True, timeout=10,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("unexpected fixture content", result.stderr)

    def test_fd_store_scope(self):
        data = Path("/repo/.mysbx-fd-stress-test/data")
        for root in STORE_ROOTS:
            with self.subTest(root=root):
                self.assertEqual(fd_scope(root, data), "store")
                self.assertEqual(fd_scope(root + "/package/file", data), "store")
                self.assertEqual(fd_scope(root + "-other/package", data), "other")
        self.assertEqual(fd_scope(str(data / "file"), data), "fixture")

    def test_fd_snapshot(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            data = root / "fixture" / "data"
            data.mkdir(parents=True)
            source = data / "file"
            source.write_bytes(b"probe\n")
            process = root / "12"
            (process / "fd").mkdir(parents=True)
            (process / "fdinfo").mkdir()
            for name, target, flags in [
                ("0", source, os.O_PATH),
                ("1", data, os.O_PATH),
                ("2", Path("/dev/null"), os.O_RDWR),
                ("3", root / "missing", os.O_PATH),
            ]:
                (process / "fd" / name).symlink_to(target)
                (process / "fdinfo" / name).write_text(f"flags:\t{flags:o}\n")
            snapshot = fd_snapshot(12, data, root)
            self.assertIn("listed=4", snapshot)
            self.assertIn("fixture file O_PATH: 1", snapshot)
            self.assertIn("fixture directory O_PATH: 1", snapshot)
            self.assertIn("other other handle: 1", snapshot)
            self.assertIn("unreadable or gone unknown unknown: 1", snapshot)

    def test_launcher_identity(self):
        self.assertTrue(is_launcher(b"/nix/store/x/bin/.mysbx-krun-wrapped\0--rootfs\0/x\0"))
        self.assertTrue(is_launcher(b"/nix/store/x/bin/mysbx-krun\0--rootfs\0/x\0"))
        self.assertFalse(is_launcher(b"/bin/python3\0mysbx-krun\0--rootfs\0"))
        self.assertFalse(is_launcher(b"mysbx-krun\0--help\0"))

    def test_stop_owned_process(self):
        proc = subprocess.Popen(
            [sys.executable, "-c", "import time; time.sleep(60)"],
            start_new_session=True,
        )
        fd = os.pidfd_open(proc.pid)
        try:
            stop(proc, fd)
            self.assertNotEqual(proc.returncode, 0)
            stop(proc, fd)
        finally:
            os.close(fd)
            stop(proc)

    def test_stop_launcher_in_separate_session(self):
        child = "import time; time.sleep(60)"
        parent = (
            "import subprocess,sys,time; "
            "p=subprocess.Popen([sys.executable,'-c',sys.argv[1]],start_new_session=True); "
            "print(p.pid,flush=True); time.sleep(60)"
        )
        proc = subprocess.Popen(
            [sys.executable, "-c", parent, child],
            stdout=subprocess.PIPE,
            text=True,
            start_new_session=True,
        )
        fd = os.pidfd_open(int(proc.stdout.readline()))
        try:
            stop(proc, fd)
            self.assertTrue(select.select([fd], [], [], 0)[0])
            self.assertNotEqual(proc.returncode, 0)
        finally:
            os.close(fd)
            proc.stdout.close()
            stop(proc)

    def test_descendants_and_missing_processes(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for pid, children, command in [
                (10, "11 99", b"/bin/bwrap\0"),
                (11, "12", b"/bin/bwrap\0"),
                (12, "", b"mysbx-krun\0--rootfs\0/x\0"),
            ]:
                task = root / str(pid) / "task" / str(pid)
                task.mkdir(parents=True)
                (task / "children").write_text(children)
                (root / str(pid) / "cmdline").write_bytes(command)
            self.assertEqual(find_launcher(10, root), 12)
            self.assertIsNone(find_launcher(99, root))
            (root / "12" / "limits").write_text(
                "Max open files            65536 65536 files\n"
            )
            self.assertEqual(read_limits(12, root), ("65536", "65536"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("repo", type=Path, nargs="?", default=Path.cwd())
    parser.add_argument("--mysbx", default=os.environ.get("MYSBX", "mysbx"))
    parser.add_argument("--files", type=int, default=100000)
    parser.add_argument("--rounds", type=int, default=3)
    parser.add_argument(
        "--fd-budget", type=int,
        help="launcher fd budget (default: 40000 for --store-scan, otherwise 8192)",
    )
    parser.add_argument("--timeout", type=float, default=600)
    parser.add_argument(
        "--store-scan",
        action="store_true",
        help="validate the guest inode watchdog (bd myconfig-dj4): walk the "
        "SHARED store, armed through a scratch repo, and require both the "
        "guest-side reclaim and a bounded host launcher fd count",
    )
    parser.add_argument(
        "--watchdog-threshold",
        type=int,
        default=20000,
        help="the MYSBX_KRUN_INODE_WATCHDOG value the scratch repo arms "
        "(store-scan mode only)",
    )
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.fd_budget is None:
        args.fd_budget = 40000 if args.store_scan else 8192
    if args.self_test:
        tests = unittest.defaultTestLoader.loadTestsFromTestCase(MonitorTests)
        result = unittest.TextTestRunner().run(tests)
        return 0 if result.wasSuccessful() else 1
    if (
        args.files < 1 or args.rounds < 1 or args.timeout <= 0
        or not math.isfinite(args.timeout) or not 0 < args.fd_budget < NOFILE
    ):
        parser.error(
            "files, rounds, and timeout must be positive; "
            "fd-budget must be between 1 and 65535"
        )
    try:
        return run(args)
    except (OSError, RuntimeError, subprocess.SubprocessError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
