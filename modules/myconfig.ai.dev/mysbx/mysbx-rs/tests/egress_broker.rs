// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Drives the BUILT `mysbx-egress-broker` binary (docs/design/egress.md,
//! bd myconfig-n8kv.1.2), so the exit codes and the channel are checked
//! end to end: `-h`/`--help` is 0, a usage error is 2, and any refusal
//! after a valid parse (a foreign run directory, a path over `SUN_LEN`, a
//! missing upstream) is 70 — never 2.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

fn broker() -> Command {
    Command::new(env!("CARGO_BIN_EXE_mysbx-egress-broker"))
}

fn run(args: &[&str]) -> (Option<i32>, String, String) {
    let out = broker()
        .args(args)
        .output()
        .expect("failed to spawn mysbx-egress-broker");
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// A fresh run directory owned by this user at mode 0700 — what mysbx
/// creates before it starts the broker (design decision E3).
fn run_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("mysbx-broker-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create the run directory");
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&dir, std::fs::Permissions::from_mode(0o700))
        .expect("chmod the run directory");
    dir
}

fn sock(dir: &Path) -> String {
    dir.join("litellm.sock").to_string_lossy().into_owned()
}

fn log_in(dir: &Path) -> String {
    dir.join("decisions.jsonl").to_string_lossy().into_owned()
}

/// A TCP fixture standing in for the host LiteLLM endpoint: it answers
/// the next connection with one fixed line.
fn upstream_fixture() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind the fixture");
    let addr = listener.local_addr().expect("fixture addr").to_string();
    std::thread::spawn(move || {
        if let Ok((mut stream, _)) = listener.accept() {
            let mut buf = [0u8; 64];
            let _ = stream.read(&mut buf);
            let _ = stream.write_all(b"HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n");
            let _ = stream.flush();
        }
    });
    addr
}

/// Spawn a broker and wait until IT has bound its channel, then return it.
///
/// Readiness is the `listen` line in the decision log, not the socket
/// file: the broker writes that line after `bind(2)` and before the first
/// accept, and a stale socket left at the path (the exec-mode debris the
/// broker replaces) would satisfy a bare "does the file exist" wait.
fn spawn_ready(dir: &Path, upstream: &str) -> (Child, String) {
    let socket = sock(dir);
    let child = broker()
        .args([
            "--listen",
            &socket,
            "--upstream",
            upstream,
            "--decision-log",
            &log_in(dir),
        ])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn mysbx-egress-broker");
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(20);
    loop {
        let bound = std::fs::read_to_string(log_in(dir))
            .map(|log| log.contains("\"event\":\"listen\""))
            .unwrap_or(false);
        if bound {
            return (child, socket);
        }
        if std::time::Instant::now() > deadline {
            let mut child = child;
            let _ = child.kill();
            let out = child.wait_with_output().expect("wait");
            panic!(
                "the broker never bound {socket}: {}",
                String::from_utf8_lossy(&out.stderr)
            );
        }
        std::thread::sleep(std::time::Duration::from_millis(25));
    }
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
    for args in [
        vec!["--bogus"],
        vec!["--listen", "/tmp/x/litellm.sock"],
        vec![
            "--listen",
            "/tmp/x/litellm.sock",
            "--upstream",
            "127.0.0.1:1",
        ],
    ] {
        let (code, _stdout, stderr) = run(&args);
        assert_eq!(code, Some(2), "{args:?}: stderr: {stderr}");
        assert!(
            stderr.contains("mysbx-egress-broker:"),
            "{args:?}: {stderr}"
        );
    }
}

#[test]
fn a_socket_path_over_sun_len_is_refused_with_seventy() {
    // A valid parse, but the computed path cannot be bound. The refusal
    // names the limit instead of surfacing a bind error (design
    // decision E4, bd myconfig-wui7).
    let long_dir = std::env::temp_dir()
        .join("a".repeat(30))
        .join("b".repeat(30))
        .join("c".repeat(30));
    std::fs::create_dir_all(&long_dir).expect("create the long run directory");
    let socket = long_dir.join("litellm.sock");
    assert!(
        socket.to_string_lossy().len() > 107,
        "the fixture path must exceed the limit: {}",
        socket.display()
    );
    let (code, _stdout, stderr) = run(&[
        "--listen",
        &socket.to_string_lossy(),
        "--upstream",
        "127.0.0.1:4000",
        "--decision-log",
        "/tmp/mysbx-broker-log-does-not-matter.jsonl",
    ]);
    let _ = std::fs::remove_dir_all(long_dir.parent().unwrap_or(Path::new("/nonexistent")));
    assert_eq!(code, Some(70), "stderr: {stderr}");
    assert!(stderr.contains("limited to 107 bytes"), "{stderr}");
    assert!(stderr.contains("E4"), "{stderr}");
}

#[test]
fn a_missing_run_directory_is_refused_with_seventy() {
    let missing = std::env::temp_dir().join("mysbx-broker-no-such-dir");
    let _ = std::fs::remove_dir_all(&missing);
    let (code, _stdout, stderr) = run(&[
        "--listen",
        &missing.join("litellm.sock").to_string_lossy(),
        "--upstream",
        "127.0.0.1:4000",
        "--decision-log",
        "/tmp/mysbx-broker-log-does-not-matter.jsonl",
    ]);
    assert_eq!(code, Some(70), "stderr: {stderr}");
    assert!(stderr.contains("does not exist"), "{stderr}");
}

#[test]
fn a_world_accessible_run_directory_is_refused_with_seventy() {
    // 0755: group/other can reach the socket directory the sandbox binds,
    // so the broker refuses to serve from it (design decision E3).
    let dir = run_dir("mode0755");
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&dir, std::fs::Permissions::from_mode(0o755))
        .expect("chmod the run directory");
    let (code, _stdout, stderr) = run(&[
        "--listen",
        &sock(&dir),
        "--upstream",
        "127.0.0.1:4000",
        "--decision-log",
        &log_in(&dir),
    ]);
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(code, Some(70), "stderr: {stderr}");
    assert!(stderr.contains("group or other"), "{stderr}");
    assert!(stderr.contains("0700"), "{stderr}");
}

#[test]
fn a_connection_is_forwarded_and_logged() {
    let dir = run_dir("forward");
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready(&dir, &addr);
    let mut client =
        std::os::unix::net::UnixStream::connect(&socket).expect("connect to the broker socket");
    client
        .write_all(b"GET /v1/models HTTP/1.1\r\nHost: localhost\r\n\r\n")
        .expect("write the request");
    let mut answer = Vec::new();
    client.read_to_end(&mut answer).expect("read the answer");
    let text = String::from_utf8_lossy(&answer);
    assert!(text.starts_with("HTTP/1.1 200"), "{text}");
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        log.contains("\"event\":\"listen\""),
        "the log must record the channel it serves: {log}"
    );
    assert!(
        log.contains("\"event\":\"allow\""),
        "the forwarded connection must be recorded: {log}"
    );
    assert!(log.contains(&addr), "{log}");
}

#[test]
fn an_unreachable_upstream_is_logged_as_unreachable_and_does_not_kill_the_broker() {
    let dir = run_dir("deny");
    // A port that was bound and released: nothing listens there now.
    let addr = {
        let probe = TcpListener::bind("127.0.0.1:0").expect("bind a probe");
        probe.local_addr().expect("probe addr").to_string()
    };
    let (child, socket) = spawn_ready(&dir, &addr);
    let mut client =
        std::os::unix::net::UnixStream::connect(&socket).expect("connect to the broker socket");
    let _ = client.write_all(b"GET /v1/models HTTP/1.1\r\n\r\n");
    // The broker drops the connection (EOF or reset), and stays up.
    let mut sink = Vec::new();
    let _ = client.read_to_end(&mut sink);
    drop(client);
    // A second connection must still be accepted: one dead upstream must
    // not end the channel.
    let second = std::os::unix::net::UnixStream::connect(&socket);
    assert!(second.is_ok(), "the broker stopped accepting: {second:?}");
    drop(second);
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(log.contains("\"event\":\"allow\""), "{log}");
    assert!(
        log.contains("\"event\":\"unreachable\""),
        "a transport failure after admission is not a policy deny: {log}"
    );
    assert!(log.contains("cannot connect"), "{log}");
}

#[test]
fn a_stale_socket_is_replaced_not_refused() {
    // An exec-mode run leaves its directory behind, so a later run on the
    // same pid finds the old socket: the broker unlinks it and binds
    // (design decision E1, the waypipe rule).
    let dir = run_dir("stale");
    let socket = sock(&dir);
    let dead = std::os::unix::net::UnixListener::bind(&socket).expect("bind a fake stale socket");
    drop(dead);
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready(&dir, &addr);
    let _ = std::os::unix::net::UnixStream::connect(&socket).expect("connect to the new socket");
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn the_decision_log_is_jsonl() {
    let dir = run_dir("jsonl");
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready(&dir, &addr);
    let mut client =
        std::os::unix::net::UnixStream::connect(&socket).expect("connect to the broker socket");
    let _ = client.write_all(b"GET /v1/models HTTP/1.1\r\n\r\n");
    let mut sink = Vec::new();
    let _ = client.read_to_end(&mut sink);
    drop(client);
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(!log.is_empty(), "the log must have lines");
    for line in log.lines() {
        assert!(line.starts_with('{') && line.ends_with('}'), "{line}");
        assert!(line.contains("\"ts\":"), "{line}");
        assert!(line.contains("\"pid\":"), "{line}");
        assert!(line.contains("\"event\":\""), "{line}");
        assert!(line.contains("\"why\":\""), "{line}");
    }
}

#[test]
fn a_non_loopback_upstream_is_refused_with_seventy() {
    // Landlock grants a PORT, not an address, so a non-loopback upstream
    // would be dialed for real: the broker refuses it (design decision E1).
    let dir = run_dir("nonloopback");
    let (code, _stdout, stderr) = run(&[
        "--listen",
        &sock(&dir),
        "--upstream",
        "10.11.12.13:4000",
        "--decision-log",
        &log_in(&dir),
    ]);
    let socket_existed = Path::new(&sock(&dir)).exists();
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(code, Some(70), "stderr: {stderr}");
    assert!(stderr.contains("not a loopback address"), "{stderr}");
    assert!(
        !socket_existed,
        "a refused upstream must not leave a socket behind"
    );
}

#[test]
fn the_fence_allows_the_pinned_port_and_refuses_every_other() {
    // The whole security claim, proven on a real kernel: after
    // `confine_to_port`, `connect(2)` reaches the allowed port and is
    // refused with EPERM on any other. The confinement is permanent for
    // the process, so it runs in a CHILD (the `#[ignore]`d test below) and
    // the parent asserts on the child's report.
    let allowed = TcpListener::bind("127.0.0.1:0").expect("bind the allowed port");
    let forbidden = TcpListener::bind("127.0.0.1:0").expect("bind the forbidden port");
    let allowed_addr = allowed.local_addr().expect("allowed addr").to_string();
    let forbidden_addr = forbidden.local_addr().expect("forbidden addr").to_string();
    // The acceptors must answer, or the allowed connect would pass for
    // the wrong reason.
    for listener in [allowed, forbidden] {
        std::thread::spawn(move || {
            for stream in listener.incoming().take(1) {
                drop(stream);
            }
        });
    }
    let out = Command::new(std::env::current_exe().expect("current exe"))
        .args([
            "--exact",
            "fence_child_allows_only_the_pinned_port",
            "--ignored",
            "--nocapture",
        ])
        .env(
            "MYSBX_FENCE_ALLOWED",
            allowed_addr.split(':').next_back().unwrap_or("0"),
        )
        .env("MYSBX_FENCE_ALLOWED_ADDR", &allowed_addr)
        .env("MYSBX_FENCE_FORBIDDEN_ADDR", &forbidden_addr)
        .output()
        .expect("run the fence child");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    if report.contains("no Landlock TCP port rules") {
        eprintln!("skipping: this kernel has no Landlock TCP port rules (ABI 4)");
        return;
    }
    assert!(out.status.success(), "the fence child failed: {report}");
    eprintln!("child report: {report}");
    assert!(report.contains("allowed: CONNECTED"), "{report}");
    assert!(report.contains("forbidden: REFUSED"), "{report}");
}

/// The child half of [`the_fence_allows_the_pinned_port_and_refuses_every_other`]:
/// confines ITSELF to the port the parent allowed, then reports what it
/// can reach. Not a test of its own — the parent runs it.
#[test]
#[ignore]
fn fence_child_allows_only_the_pinned_port() {
    let Ok(port) = std::env::var("MYSBX_FENCE_ALLOWED") else {
        return;
    };
    let port: u16 = port.parse().expect("the allowed port");
    let allowed_addr = std::env::var("MYSBX_FENCE_ALLOWED_ADDR").expect("the allowed address");
    let forbidden_addr =
        std::env::var("MYSBX_FENCE_FORBIDDEN_ADDR").expect("the forbidden address");
    match mysbx::landlock::abi() {
        Ok(abi) if abi >= mysbx::landlock::ABI_NET_PORTS => {}
        Ok(abi) => {
            eprintln!("no Landlock TCP port rules (ABI {abi})");
            return;
        }
        Err(e) => {
            eprintln!("no Landlock TCP port rules ({e})");
            return;
        }
    }
    mysbx::landlock::confine_to_tcp_port(port).expect("confine to the allowed port");
    match TcpStream::connect(&allowed_addr) {
        Ok(_) => println!("allowed: CONNECTED"),
        Err(e) => println!("allowed: FAILED ({e})"),
    }
    match TcpStream::connect(&forbidden_addr) {
        Ok(_) => println!("forbidden: CONNECTED"),
        Err(e) => println!("forbidden: REFUSED ({e})"),
    }
}

fn the_forwarded_connection_still_carries_the_client_bytes() {
    // The upstream fixture answers only after it read a request, so a
    // broker that dropped the request direction would leave the client
    // with a truncated (or empty) answer.
    let dir = run_dir("bytes");
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind the fixture");
    let addr = listener.local_addr().expect("fixture addr").to_string();
    let seen = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    let seen_in_thread = std::sync::Arc::clone(&seen);
    std::thread::spawn(move || {
        if let Ok((mut stream, _)) = listener.accept() {
            let mut buf = [0u8; 128];
            if let Ok(n) = stream.read(&mut buf) {
                seen_in_thread.lock().unwrap().extend_from_slice(&buf[..n]);
            }
            let _ = stream.write_all(b"HTTP/1.1 200 OK\r\n\r\nok");
        }
    });
    let (child, socket) = spawn_ready(&dir, &addr);
    let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
    client
        .write_all(b"POST /v1/chat/completions HTTP/1.1\r\nContent-Length: 2\r\n\r\n{}")
        .expect("write");
    let mut answer = Vec::new();
    let _ = client.read_to_end(&mut answer);
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let _ = std::fs::remove_dir_all(&dir);
    let request = String::from_utf8_lossy(&seen.lock().unwrap()).into_owned();
    assert!(request.contains("POST /v1/chat/completions"), "{request}");
    assert!(String::from_utf8_lossy(&answer).contains("200 OK"));
}
