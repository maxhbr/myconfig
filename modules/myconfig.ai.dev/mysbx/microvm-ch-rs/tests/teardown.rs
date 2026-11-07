// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The teardown contract of the CH launcher (bd myconfig-1ct7, the
//! qemu twin's test adapted): cleanup lives in Daemons::drop —
//! kill the virtiofsd AND passt children, sweep the per-run
//! runtime dir — so the run path must NEVER process::exit inside
//! the guard's scope. These tests drive the REAL built launcher
//! with daemon STUBS (shell scripts that create the socket and
//! sleep) and a dead CH pin: the failure path must return 125 AND
//! leave no runtime dir and no stub process behind. The
//! success-side sweep needs a real CH and lives in the
//! live-validation runbook.

use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

/// The tests each drive a REAL launcher whose runtime dir lives in
/// the SAME temp dir — the dir-sweep assertion is only sound when
/// no other launcher of this suite is mid-run, so the tests
/// serialize on this lock.
fn suite_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

const EXIT_SETUP: i32 = 125;

fn stub_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "mysbx-ch-teardown-test-{name}-{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("the stub dir");
    dir
}

/// A stub that serves the socket flag it is handed, records its
/// pid and EXECs sleep — with exec, the recorded pid names the
/// exact process the launcher's Daemons::drop kills (pidfile
/// semantics mirror "did the drop reap its child"; a non-exec
/// stub would leave `sleep` as sh's child, which a single kill()
/// can never reach, bd myconfig-tjwy).
fn socket_stub(dir: &PathBuf, name: &str) -> PathBuf {
    let path = dir.join(format!("{name}-stub.sh"));
    let pidfile = dir.join(format!("{name}-stub.pid"));
    std::fs::write(
        &path,
        format!(
            "#!/bin/sh
# A stub daemon: create the socket named by --socket-path=... (or
# --socket=... for passt), record the pid, then serve (sleep)
# until killed — a process that OUTLIVES the launcher if the
# teardown is broken.
for a in \"$@\"; do
  case \"$a\" in
    --socket-path=*|--socket=*) s=${{a#*=}}; : > \"$s\" ;;
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
/// nix build sandbox has no `ps` on PATH (the qemu twin's own
/// finding), and /proc is the source `ps` would read anyway.
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

/// No runtime dir of the launcher's exact shape
/// (`mysbx-microvm-ch-<pid>`, digits only) may exist after a run
/// returned.
fn no_runtime_dir() -> bool {
    std::fs::read_dir(std::env::temp_dir())
        .map(|it| {
            it.filter_map(|e| e.ok())
                .filter(|e| {
                    let name = e.file_name();
                    let name = name.to_string_lossy();
                    match name.strip_prefix("mysbx-microvm-ch-") {
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
    passt: &std::path::Path,
    ch: &str,
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
        .env("MYSBX_MICROVM_CH_VIRTIOFSD", virtiofsd)
        .env("MYSBX_MICROVM_CH_PASST", passt)
        .env("MYSBX_MICROVM_CH_CH", ch)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("the launcher runs")
}

/// A stub virtiofsd that never creates its socket: the readiness
/// wait must time out (100 × 10 ms), the run must REFUSE with 125,
/// and the already-spawned stubs plus the runtime dir must be gone
/// when the launcher returns.
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
# the sleeper.
exec sleep 600
",
    )
    .expect("the silent stub");
    make_executable(&virtiofsd);
    let passt = socket_stub(&dir, "passt");
    let output = run_launcher(
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-ch")).as_path(),
        &virtiofsd,
        &passt,
        "/does/not/matter/while-readiness-fails",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(EXIT_SETUP), "{stderr}");
    assert!(stderr.contains("timed out waiting for the"), "{stderr}");
    assert!(no_runtime_dir(), "the runtime dir survived the refusal");
}

/// A live stub daemon set and a DEAD CH pin: the spawn failure
/// happens with every daemon serving — the drop must still kill
/// them all and sweep the dir (the passt stub shares the socket
/// contract, so `--network shared` exercises its child slot too).
#[test]
fn a_ch_spawn_failure_kills_the_daemons_and_sweeps_the_dir() {
    let _guard = suite_lock().lock().unwrap_or_else(|e| e.into_inner());
    let dir = stub_dir("ch-dead-pin");
    let virtiofsd = socket_stub(&dir, "virtiofsd");
    let passt = socket_stub(&dir, "passt");
    let output = run_launcher(
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-ch")).as_path(),
        &virtiofsd,
        &passt,
        "/does/not/exist/cloud-hypervisor",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("daemons ready"), "{stderr}");
    assert!(stderr.contains("cannot spawn cloud-hypervisor"), "{stderr}");
    assert_eq!(output.status.code(), Some(EXIT_SETUP), "{stderr}");
    assert!(no_runtime_dir(), "the runtime dir survived the refusal");
}

/// The stub processes themselves: nothing the launcher spawned may
/// outlive it (the kill + reap is the drop's first half — a leak
/// here means the guard never ran). The passt stub is the witness:
/// it is the LAST child of the readiness phase, so its survival
/// would mean the whole kill loop skipped. The check reads the
/// stub's own pidfile straight from /proc. Serialized on the suite
/// lock like the other tests.
#[test]
fn no_stub_daemon_survives_a_failed_run() {
    let _guard = suite_lock().lock().unwrap_or_else(|e| e.into_inner());
    let dir = stub_dir("stub-survival");
    let virtiofsd = socket_stub(&dir, "virtiofsd");
    let passt = socket_stub(&dir, "passt");
    let pidfile = dir.join("passt-stub.pid");
    // Negative control FIRST: spawn the stub directly, prove the
    // pidfile + /proc check can actually see a live stub — so the
    // assertion below cannot regress into another vacuous pass.
    let mut control = Command::new(&passt)
        .arg(format!("--socket={}", dir.join("control.sock").display()))
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
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-ch")).as_path(),
        &virtiofsd,
        &passt,
        "/does/not/exist/cloud-hypervisor",
    );
    let pid = read_pid(&pidfile).expect("no stub recorded its pid — the fixture is broken");
    let deadline = Instant::now() + Duration::from_secs(2);
    while pid_alive(pid) {
        if Instant::now() > deadline {
            panic!("a passt stub (pid {pid}) survived the launcher's teardown");
        }
        std::thread::sleep(Duration::from_millis(100));
    }
}

/// A guest panic hangs cloud-hypervisor (no -no-reboot): a stub CH
/// that writes CH 54's pvpanic event to its --event-monitor file and
/// then hangs must be killed by the launcher's crash watch within
/// seconds — the run reports the panic (125) instead of waiting out
/// the teardown timeout, and the daemons and runtime dir are gone.
#[test]
fn a_guest_panic_kills_the_hung_vm_promptly() {
    let _guard = suite_lock().lock().unwrap_or_else(|e| e.into_inner());
    let dir = stub_dir("panic");
    let virtiofsd = socket_stub(&dir, "virtiofsd");
    let passt = socket_stub(&dir, "passt");
    let ch = dir.join("ch-panic.sh");
    std::fs::write(
        &ch,
        "#!/bin/sh
# A stub CH whose guest panics: the pvpanic event lands on the
# event monitor (serde_json's pretty form), then the VM hangs —
# EXEC so the launcher's kill reaches the sleeper itself.
for a in \"$@\"; do
  case \"$a\" in
    --event-monitor=path=*) ev=${a#--event-monitor=path=} ;;
  esac
done
printf '{\\n  \"source\": \"guest\",\\n  \"event\": \"panic\",\\n  \"properties\": {\\n    \"event\": \"panic\"\\n  }\\n}\\n' >> \"$ev\"
exec sleep 600
",
    )
    .expect("the panic stub");
    make_executable(&ch);
    let started = Instant::now();
    let output = run_launcher(
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-ch")).as_path(),
        &virtiofsd,
        &passt,
        ch.to_str().expect("a utf-8 stub path"),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        started.elapsed() < Duration::from_secs(30),
        "the hung VM was not killed promptly: {stderr}"
    );
    assert!(stderr.contains("the guest PANICKED"), "{stderr}");
    assert_eq!(output.status.code(), Some(EXIT_SETUP), "{stderr}");
    assert!(no_runtime_dir(), "the runtime dir survived the panic");
}

/// A stub virtiofsd that logs more than a pipe holds (64 KiB) BEFORE
/// it serves: the launcher must drain the daemon's stderr, or the
/// stub blocks on the full pipe, never creates its socket, and the
/// run fails readiness.
#[test]
fn a_chatty_daemon_does_not_block_on_its_stderr_pipe() {
    let _guard = suite_lock().lock().unwrap_or_else(|e| e.into_inner());
    let dir = stub_dir("chatty");
    let virtiofsd = dir.join("virtiofsd-chatty.sh");
    std::fs::write(
        &virtiofsd,
        "#!/bin/sh
yes 'virtiofsd chatter' | head -n 20000 >&2
for a in \"$@\"; do
  case \"$a\" in
    --socket-path=*) s=${a#--socket-path=}; : > \"$s\" ;;
  esac
done
exec sleep 600
",
    )
    .expect("the chatty stub");
    make_executable(&virtiofsd);
    let passt = socket_stub(&dir, "passt");
    let output = run_launcher(
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-ch")).as_path(),
        &virtiofsd,
        &passt,
        "/does/not/exist/cloud-hypervisor",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("daemons ready"), "{stderr}");
    assert!(!stderr.contains("virtiofsd chatter"), "{stderr}");
    assert_eq!(output.status.code(), Some(EXIT_SETUP), "{stderr}");
    assert!(no_runtime_dir(), "the runtime dir survived the refusal");
}

/// A daemon that fails to serve: its last stderr lines ride the
/// readiness refusal, so the cause is visible without --verbose.
#[test]
fn a_readiness_failure_reports_the_daemons_stderr() {
    let _guard = suite_lock().lock().unwrap_or_else(|e| e.into_inner());
    let dir = stub_dir("readiness-log");
    let virtiofsd = dir.join("virtiofsd-failing.sh");
    std::fs::write(
        &virtiofsd,
        "#!/bin/sh
echo 'virtiofsd-stub: cannot open the shared directory' >&2
exec sleep 600
",
    )
    .expect("the failing stub");
    make_executable(&virtiofsd);
    let passt = socket_stub(&dir, "passt");
    let output = run_launcher(
        PathBuf::from(env!("CARGO_BIN_EXE_mysbx-microvm-ch")).as_path(),
        &virtiofsd,
        &passt,
        "/does/not/matter/while-readiness-fails",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(EXIT_SETUP), "{stderr}");
    assert!(
        stderr.contains("virtiofsd-stub: cannot open the shared directory"),
        "{stderr}"
    );
    assert!(no_runtime_dir(), "the runtime dir survived the refusal");
}
