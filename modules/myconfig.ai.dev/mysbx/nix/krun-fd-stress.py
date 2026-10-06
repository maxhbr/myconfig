#!/usr/bin/env python3
"""Run a controlled shared-tree walk and measure the host launcher's fd count."""

import argparse
import base64
from collections import Counter
import math
import os
from pathlib import Path
import resource
import select
import shlex
import signal
import stat
import subprocess
import sys
import tempfile
import time
import unittest

NOFILE = 65536
STORE_ROOTS = ("/nix/store", "/mysbx-krun-stage/ro/store")
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
    parser.add_argument("--fd-budget", type=int, default=8192)
    parser.add_argument("--timeout", type=float, default=600)
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
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
