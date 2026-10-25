// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The teardown contract (bd myconfig-1ct7): cleanup lives in
//! Daemons::drop — kill the virtiofsd children, sweep the per-run
//! runtime dir — so the run path must NEVER process::exit inside
//! the guard's scope. These tests drive the REAL built launcher
//! with a virtiofsd STUB (a shell script that creates the socket
//! and sleeps) and a dead qemu pin: the failure path must return
//! 125 AND leave no runtime dir and no stub process behind. The
//! success-side sweep needs a real qemu and lives in the
//! live-validation runbook (bd myconfig-26z4).

use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

/// The three tests each drive a REAL launcher whose runtime dir
/// lives in the SAME temp dir — the dir-sweep assertion is only
/// sound when no other launcher of this suite is mid-run, so the
/// tests serialize on this lock.
fn suite_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

const EXIT_SETUP: i32 = 125;

fn stub_dir(name: &str) -> PathBuf {
    let dir =
        std::env::temp_dir().join(format!("mysbx-teardown-test-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("the stub dir");
    dir
}

fn virtiofsd_stub(dir: &PathBuf) -> PathBuf {
    let path = dir.join("virtiofsd-stub.sh");
    // The stub records its own pid ($$) and EXECs sleep: with exec,
    // the recorded pid names the exact process the launcher's
    // Daemons::drop kills — pidfile semantics mirror "did the drop
    // reap its child". A non-exec stub would leave `sleep` as sh's
    // child, which a single kill() can never reach (bd
    // myconfig-tjwy).
    let pidfile = dir.join("virtiofsd-stub.pid");
    std::fs::write(
        &path,
        format!(
            "#!/bin/sh
# A stub virtiofsd: create the socket named by --socket-path=...,
# record the pid, then serve (sleep) until killed — a process that
# OUTLIVES the launcher if the teardown is broken.
for a in \"$@\"; do
  case \"$a\" in
    --socket-path=*) s=${{a#--socket-path=}}; : > \"$s\" ;;
  esac
done
echo $$ > {pidfile}
exec sleep 600
",
            pidfile = pidfile.display()
        ),
    )
    .expect("the stub script");
    make_executable(&path);
    path
}

/// Whether a process is still alive, read from /proc DIRECTLY — a
/// nix build sandbox has no `ps` on PATH (the thing build
/// 2026-10-24 caught it), and /proc is the source `ps` would read
/// anyway (bd myconfig-p9zu).
fn pid_alive(pid: u32) -> bool {
    std::path::Path::new(&format!("/proc/{pid}")).exists()
}

/// Read the stub's pidfile, retrying briefly: the stub records its
/// pid moments after spawn, and the reader must not race that.
fn read_pid(pidfile: &std::path::Path) -> Option<u32> {
    let deadline = Instant::now() + Duration::from_secs(2);
    loop {
        if let Some(pid) = std::fs::read_to_string(pidfile)
            .ok()
            .and_then(|s| s.trim().parse::<u32>().ok())
        {
            return Some(pid);
        }
        if Instant::now() > deadline {
            return None;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
}

#[cfg(unix)]
fn make_executable(path: &std::path::Path) {
    use std::os::unix::fs::PermissionsExt;
    let mut perm = std::fs::metadata(path)
        .expect("the stub's metadata")
        .permissions();
    perm.set_mode(0o755);
    std::fs::set_permissions(path, perm).expect("the stub's mode");
}

#[cfg(not(unix))]
fn make_executable(_path: &std::path::Path) {}

/// No runtime dir of the launcher's exact shape (`mysbx-microvm-qemu-<pid>`,
/// digits only) may exist after a run returned: the pid names the
/// dir, so another test's LIVE dir would false-positive on a bare
/// prefix match — the shape check keeps the assertion precise.
fn no_runtime_dir() -> bool {
    std::fs::read_dir(std::env::temp_dir())
        .map(|it| {
            it.filter_map(|e| e.ok())
                .filter(|e| {
                    let name = e.file_name();
                    let name = name.to_string_lossy();
                    match name.strip_prefix("mysbx-microvm-qemu-") {
                        Some(pid) => !pid.is_empty() && pid.bytes().all(|b| b.is_ascii_digit()),
                        None => false,
                    }
                })
                .next()
                .is_none()
        })
        .unwrap_or(true)
}

fn run_launcher(
    launcher: &std::path::Path,
    virtiofsd: &std::path::Path,
    qemu: &str,
) -> std::process::Output {
    let rootfs = stub_dir("rootfs");
    let stage = stub_dir("stage");
    Command::new(launcher)
        .arg("--rootfs")
        .arg(&rootfs)
        .arg("--kernel")
        .arg("/does/not/matter/bzImage")
        .arg("--ro-device")
        .arg(format!("stage-ro={}", stage.display()))
        .arg("--manifest")
        .arg(format!("{}/manifest", stage.display()))
        .arg("--")
        .arg("/bin/true")
        .env("MYSBX_MICROVM_QEMU_VIRTIOFSD", virtiofsd)
        .env("MYSBX_MICROVM_QEMU_QEMU", qemu)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("the launcher runs")
}

/// A stub virtiofsd that never creates its socket: the readiness
/// wait must time out (100 × 10 ms), the run must REFUSE with 125,
/// and the already-spawned stub plus the runtime dir must be gone
/// when the launcher returns — the failure path the old
/// process::exit code skipped the guard on.
#[test]
fn a_readiness_timeout_kills_the_daemons_and_sweeps_the_dir() {
    let _guard = suite_lock().lock().unwrap_or_else(|e| e.into_inner());
    let dir = stub_dir("readiness");
    let virtiofsd = dir.join("virtiofsd-silent.sh");
    std::fs::write(
        &virtiofsd,
        "#!/bin/sh
# A stub virtiofsd that serves NOTHING: no socket file, just a
# sleep the launcher's kill must reach — EXEC so the killed pid IS
# the sleeper (a bare child sleep would be reparented and outlive
# the launcher, bd myconfig-6mkm).
exec sleep 600
",
    )
    .expect("the silent stub");
    make_executable(&virtiofsd);
    let output = run_launcher(
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-qemu")).as_path(),
        &virtiofsd,
        "/does/not/matter/while-readiness-fails",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(EXIT_SETUP), "{stderr}");
    assert!(stderr.contains("timed out waiting for the"), "{stderr}");
    assert!(no_runtime_dir(), "the runtime dir survived the refusal");
}

/// A live stub daemon set and a DEAD qemu pin: the spawn failure
/// happens with every daemon serving — the old code exited without
/// the drop and orphuned them all.
#[test]
fn a_qemu_spawn_failure_kills_the_daemons_and_sweeps_the_dir() {
    let _guard = suite_lock().lock().unwrap_or_else(|e| e.into_inner());
    let dir = stub_dir("qemu-dead-pin");
    let virtiofsd = virtiofsd_stub(&dir);
    let output = run_launcher(
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-qemu")).as_path(),
        &virtiofsd,
        "/does/not/exist/qemu-system-x86_64",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("2 virtiofsd daemons ready"), "{stderr}");
    assert!(stderr.contains("cannot spawn qemu"), "{stderr}");
    assert_eq!(output.status.code(), Some(EXIT_SETUP), "{stderr}");
    assert!(no_runtime_dir(), "the runtime dir survived the refusal");
}

/// The stub processes themselves: nothing the launcher spawned may
/// outlive it by two seconds (the kill + reap is the drop's first
/// half — a leak here means the guard never ran). The check reads
/// the stub's own pidfile (recorded before exec sleep, so the pid
/// IS the sleeper) straight from /proc — no ps, and no
/// cross-process string greps that can never co-occur in one
/// process's args (bd myconfig-tjwy/p9zu). Serialized on the
/// suite lock like the other two: the runtime-dir scan only holds
/// while no other launcher of this suite is mid-run.
#[test]
fn no_stub_daemon_survives_a_failed_run() {
    let _guard = suite_lock().lock().unwrap_or_else(|e| e.into_inner());
    let dir = stub_dir("stub-survival");
    let virtiofsd = virtiofsd_stub(&dir);
    let pidfile = dir.join("virtiofsd-stub.pid");
    // Negative control FIRST: spawn the stub directly, prove the
    // pidfile + /proc check can actually see a live stub — so the
    // assertion below cannot regress into another vacuous pass.
    let mut control = Command::new(&virtiofsd)
        .arg(format!("--socket-path={}", dir.join("control").display()))
        .spawn()
        .expect("the control stub spawns");
    let control_pid = read_pid(&pidfile).expect("the control stub records its pid");
    assert!(
        pid_alive(control_pid),
        "the /proc check cannot see a live stub — the leak check is vacuous"
    );
    let _ = control.kill();
    let _ = control.wait();
    let _ = std::fs::remove_file(&pidfile);

    let _output = run_launcher(
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-qemu")).as_path(),
        &virtiofsd,
        "/does/not/exist/qemu-system-x86_64",
    );
    // The launcher spawns one virtiofsd per device (root + stage =
    // two stubs) but they share the pidfile path — the last writer
    // wins, and both had the same fate in the same kill loop, so
    // the recorded one is a sound witness.
    let pid = read_pid(&pidfile).expect("no stub recorded its pid — the fixture is broken");
    let deadline = Instant::now() + Duration::from_secs(2);
    while pid_alive(pid) {
        if Instant::now() > deadline {
            panic!("a virtiofsd stub (pid {pid}) survived the launcher's teardown");
        }
        std::thread::sleep(Duration::from_millis(100));
    }
}
