// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Drives the BUILT `mysbx-egress-relay` binary, so the exit codes are
//! checked end to end (docs/design/config.md D20, bd myconfig-mo3.2.5):
//! `-h`/`--help` is 0, a usage error is 2, and any runtime failure after
//! a valid parse (a bind on a taken port, a payload that is not there) is
//! 70 — never 2.

use std::net::TcpListener;
use std::process::Command;

fn relay() -> Command {
    Command::new(env!("CARGO_BIN_EXE_mysbx-egress-relay"))
}

fn run(args: &[&str]) -> (Option<i32>, String, String) {
    let out = relay()
        .args(args)
        .output()
        .expect("failed to spawn mysbx-egress-relay");
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[test]
fn help_prints_usage_to_stdout_and_exits_zero() {
    for flag in ["-h", "--help"] {
        let (code, stdout, stderr) = run(&[flag]);
        assert_eq!(code, Some(0), "{flag}: stderr: {stderr}");
        assert!(stdout.contains("usage:"), "{flag}: {stdout}");
        assert!(stderr.is_empty(), "{flag}: {stderr}");
    }
}

#[test]
fn a_usage_error_exits_two() {
    // An unknown flag, and a missing required one: both are usage
    // errors (exit 2), not runtime failures.
    for args in [
        vec!["--bogus"],
        vec!["--listen", "127.0.0.1:1"],
        vec!["--listen", "127.0.0.1:1", "--connect", "/x", "--"],
    ] {
        let (code, _stdout, stderr) = run(&args);
        assert_eq!(code, Some(2), "{args:?}: stderr: {stderr}");
        assert!(stderr.contains("mysbx-egress-relay:"), "{args:?}: {stderr}");
    }
}

#[test]
fn a_taken_port_is_a_runtime_failure_exit_seventy() {
    // A valid parse, but the port is already bound: exit 70, one stderr
    // line — not exit 2.
    let taken = TcpListener::bind("127.0.0.1:0").expect("bind an ephemeral port");
    let addr = taken.local_addr().expect("local addr").to_string();
    let (code, _stdout, stderr) = run(&[
        "--listen",
        &addr,
        "--connect",
        "/tmp/mysbx-egress-relay-does-not-matter.sock",
        "--",
        "/bin/true",
    ]);
    assert_eq!(code, Some(70), "stderr: {stderr}");
    assert!(stderr.contains("cannot bind"), "{stderr}");
}

#[test]
fn a_missing_payload_is_a_runtime_failure_exit_seventy() {
    // A valid parse, but the payload binary does not exist: exit 70.
    let (code, _stdout, stderr) = run(&[
        "--listen",
        "127.0.0.1:0",
        "--connect",
        "/tmp/mysbx-egress-relay-does-not-matter.sock",
        "--",
        "/nonexistent/mysbx-relay-payload",
    ]);
    assert_eq!(code, Some(70), "stderr: {stderr}");
    assert!(stderr.contains("cannot start"), "{stderr}");
}

#[test]
fn a_sigint_sent_to_the_relay_reaches_the_payload() {
    // `kill -INT <relay>` (si_code SI_USER) targets the relay alone, so
    // it must be forwarded: the payload traps SIGINT and exits 42, and
    // the relay passes that status through. A terminal's ^C (SI_KERNEL)
    // is not forwarded; that case needs a pty and is not covered here.
    let mut child = relay()
        .args([
            "--listen",
            "127.0.0.1:0",
            "--connect",
            "/tmp/mysbx-egress-relay-does-not-matter.sock",
            "--",
            "/bin/sh",
            "-c",
            "trap 'exit 42' INT; echo ready; while :; do :; done",
        ])
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn mysbx-egress-relay");
    // Wait until the payload has installed its trap.
    let mut ready = String::new();
    std::io::BufRead::read_line(
        &mut std::io::BufReader::new(child.stdout.take().expect("stdout")),
        &mut ready,
    )
    .expect("read the payload's ready line");
    assert_eq!(ready.trim(), "ready");
    unsafe { mysbx::sig::kill_pid(child.id() as i32, mysbx::sig::INT) };
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    let status = loop {
        if let Some(status) = child.try_wait().expect("try_wait") {
            break status;
        }
        if std::time::Instant::now() > deadline {
            let _ = child.kill();
            let _ = child.wait();
            panic!("the payload never got the forwarded SIGINT");
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    };
    assert_eq!(status.code(), Some(42), "status: {status:?}");
}
