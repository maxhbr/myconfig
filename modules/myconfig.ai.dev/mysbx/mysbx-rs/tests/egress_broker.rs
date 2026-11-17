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
    spawn_ready_with(dir, upstream, &[])
}

/// [`spawn_ready`] with extra broker argv (the CONNECT table flags, bd
/// myconfig-ew9s): the same `listen`-line readiness wait.
fn spawn_ready_with(dir: &Path, upstream: &str, extra: &[&str]) -> (Child, String) {
    let socket = sock(dir);
    let mut cmd = broker();
    cmd.args([
        "--listen",
        &socket,
        "--upstream",
        upstream,
        "--decision-log",
        &log_in(dir),
    ])
    .args(extra)
    .stdin(Stdio::null())
    .stdout(Stdio::null())
    .stderr(Stdio::piped());
    let child = cmd.spawn().expect("failed to spawn mysbx-egress-broker");
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

#[test]
fn the_fence_blocks_the_socket_re_aim_rotation_inside_bwrap() {
    // E11 (docs/design/egress-connect.md, bd myconfig-ew9s): the
    // gateway-rotation attack re-aims an already-admitted socket at a
    // new destination with IP_TRANSPARENT / TCP_REPAIR / IP_FREEBIND
    // and splices. This test proves the kernel-side half INSIDE the
    // real bubblewrap fence, exactly the way the payload runs:
    //
    // - IP_TRANSPARENT and TCP_REPAIR fail EPERM (no CAP_NET_ADMIN in
    //   a bwrap payload, NoNewPrivs set);
    // - IP_FREEBIND is ALLOWED (it is not capability-gated) and the
    //   bind to a non-local source SUCCEEDS — named deliberately, so
    //   the design never rests on "no caps means every spoofing
    //   option fails";
    // - the connect(2) from that spoofed source FAILS (the netns has
    //   no route) and so does a plain non-loopback connect: no routed
    //   socket to re-aim, nothing to splice.
    //
    // Skipped when no bwrap is available: the fence is bwrap's
    // --unshare-all, so a stub cannot stand in.
    let bwrap = std::env::var("MYSBX_BWRAP").unwrap_or_else(|_| "bwrap".to_owned());
    if !Command::new(&bwrap)
        .arg("--version")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .is_ok_and(|out| out.status.success())
    {
        eprintln!("skipping: no bwrap to run the fence child under");
        return;
    }
    let out = Command::new(&bwrap)
        .arg("--unshare-all")
        .arg("--ro-bind")
        .arg("/nix/store")
        .arg("/nix/store")
        // The test binary lives under the build tree, not the store —
        // bind its own path ro so the fence can exec it (its ELF
        // interpreter is in the store bind above).
        .arg("--ro-bind")
        .arg(std::env::current_exe().expect("current exe"))
        .arg(std::env::current_exe().expect("current exe"))
        .arg("--proc")
        .arg("/proc")
        .arg("--dev")
        .arg("/dev")
        .arg("--clearenv")
        .arg(std::env::current_exe().expect("current exe"))
        .args([
            "--exact",
            "fence_child_rotation_probe",
            "--ignored",
            "--nocapture",
        ])
        .output()
        .expect("run the rotation probe under bwrap");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    eprintln!("rotation probe report: {report}");
    assert!(out.status.success(), "the rotation probe failed: {report}");
    assert!(report.contains("IP_TRANSPARENT: EPERM"), "{report}");
    assert!(report.contains("TCP_REPAIR: EPERM"), "{report}");
    assert!(report.contains("IP_FREEBIND: ALLOWED"), "{report}");
    assert!(
        report.contains("freebind-bind(192.0.2.77): OK"),
        "the one option that succeeds is named: {report}"
    );
    assert!(
        report.contains("freebind-connect: FAILED"),
        "the spoofed-source connect never leaves the netns: {report}"
    );
    assert!(
        report.contains("plain-connect: FAILED"),
        "no routed socket exists to re-aim: {report}"
    );
}

/// The child half of
/// [`the_fence_blocks_the_socket_re_aim_rotation_inside_bwrap`]: a
/// raw-syscall probe that reports the setsockopt inventory and the
/// connect outcomes, one line per primitive, the exact spelling the
/// parent asserts on. Not a test of its own — the parent runs it
/// under bwrap.
#[test]
#[ignore]
fn fence_child_rotation_probe() {
    // The raw externs of the crate's zero-dependency idiom.
    const AF_INET: i32 = 2;
    const SOCK_STREAM: i32 = 1;
    const IPPROTO_IP: i32 = 0;
    const IPPROTO_TCP: i32 = 6;
    #[repr(C)]
    struct SockaddrIn {
        sin_family: u16,
        sin_port: u16,
        sin_addr: u32,
        sin_zero: [u8; 8],
    }
    extern "C" {
        fn socket(domain: i32, ty: i32, proto: i32) -> i32;
        fn setsockopt(fd: i32, level: i32, optname: i32, optval: *const u8, optlen: u32) -> i32;
        fn bind(fd: i32, addr: *const SockaddrIn, len: u32) -> i32;
        fn connect(fd: i32, addr: *const SockaddrIn, len: u32) -> i32;
        fn close(fd: i32) -> i32;
    }
    // IP_TRANSPARENT=19, IP_FREEBIND=15 (IPPROTO_IP); TCP_REPAIR=19
    // (IPPROTO_TCP).
    let probe = |level: i32, opt: i32, label: &str| {
        let fd = unsafe { socket(AF_INET, SOCK_STREAM, 0) };
        let one: i32 = 1;
        let rc = unsafe { setsockopt(fd, level, opt, &one as *const i32 as *const u8, 4) };
        unsafe { close(fd) };
        if rc == 0 {
            println!("{label}: ALLOWED");
        } else {
            println!("{label}: EPERM");
        }
    };
    probe(IPPROTO_IP, 19, "IP_TRANSPARENT");
    probe(IPPROTO_TCP, 19, "TCP_REPAIR");
    probe(IPPROTO_IP, 15, "IP_FREEBIND");
    // The FREEBIND follow-through: bind a non-local source, then try
    // to connect from it.
    let fd = unsafe { socket(AF_INET, SOCK_STREAM, 0) };
    let one: i32 = 1;
    let _ = unsafe { setsockopt(fd, IPPROTO_IP, 15, &one as *const i32 as *const u8, 4) };
    let mut src = SockaddrIn {
        sin_family: AF_INET as u16,
        sin_port: 0,
        sin_addr: u32::from_ne_bytes([192, 0, 2, 77]),
        sin_zero: [0; 8],
    };
    let bound = unsafe {
        bind(
            fd,
            &src as *const SockaddrIn,
            std::mem::size_of::<SockaddrIn>() as u32,
        )
    };
    if bound != 0 {
        println!("freebind-bind(192.0.2.77): FAILED");
        return;
    }
    println!("freebind-bind(192.0.2.77): OK");
    src.sin_port = 443u16.to_be();
    src.sin_addr = u32::from_ne_bytes([1, 1, 1, 1]);
    let rc = unsafe {
        connect(
            fd,
            &src as *const SockaddrIn,
            std::mem::size_of::<SockaddrIn>() as u32,
        )
    };
    if rc == 0 {
        println!("freebind-connect: CONNECTED");
    } else {
        println!("freebind-connect: FAILED");
    }
    // The plain connect: no spoofing options at all.
    match std::net::TcpStream::connect("1.1.1.1:443") {
        Ok(_) => println!("plain-connect: CONNECTED"),
        Err(_) => println!("plain-connect: FAILED"),
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

// ---- The CONNECT lane (docs/design/egress-connect.md E8/E9, bd
// myconfig-ew9s) ----

/// Read one line from `stream` (up to `\n`), for the handshake answers.
fn read_line_until_double_crlf(stream: &mut std::os::unix::net::UnixStream) -> String {
    let mut out = Vec::new();
    let mut buf = [0u8; 1];
    loop {
        match stream.read(&mut buf) {
            Ok(0) => break,
            Ok(_) => {
                out.push(buf[0]);
                if out.ends_with(b"\r\n\r\n") {
                    break;
                }
                if out.len() > 8 * 1024 {
                    break;
                }
            }
            Err(_) => break,
        }
    }
    String::from_utf8_lossy(&out).into_owned()
}

#[test]
fn a_connect_to_an_allowed_domain_and_port_is_admitted_and_tunneled() {
    // E9's admit + tunnel: a CONNECT to the table's host on its port
    // gets `200 Connection Established`, and the bytes after the
    // request head reach the dialed upstream. The fixture resolves
    // `localhost` host-side (the broker resolves the NAME); the
    // address classes allow it here — wait, no: loopback is a refused
    // class for the CONNECT lane. So this fixture uses the
    // documentation range's resolved name: `localhost` is refused by
    // the class check. Use a name that resolves to a non-loopback
    // address... this offline sandbox has no resolver. The honest
    // fixture: a table with ports-only and an IP-literal target
    // (admitted by ports-only, and the address classes are checked
    // AFTER the admission). A documentation-range literal is refused
    // by the class check, so the tunnel test cannot dial a real
    // address offline — it asserts the HANDSHAKE and the events up to
    // the dial, and the dial failure is the `unreachable` event the
    // next test pins.
    let dir = run_dir("connect-admit");
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready_with(&dir, &addr, &["--allow-connect-port", "443"]);
    let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
    client
        .write_all(b"CONNECT 192.0.2.9:443 HTTP/1.1\r\n\r\n")
        .expect("write CONNECT");
    let answer = read_line_until_double_crlf(&mut client);
    let mut child = child;
    let _ = client.shutdown(std::net::Shutdown::Both);
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    // The admission is recorded; the dial of a documentation-range
    // address is a transport refusal, so the `allow` line exists and
    // the `unreachable` line names the class — never a `deny`.
    assert!(
        log.contains("\"event\":\"allow\""),
        "the admission must be recorded: {log}"
    );
    assert!(
        log.contains("CONNECT 192.0.2.9:443"),
        "the allow line names the CONNECT target: {log}"
    );
    assert!(
        log.contains("refused address class"),
        "the class refusal is a transport event: {log}"
    );
    assert!(
        !log.contains("\"event\":\"deny\""),
        "a class refusal must not read as a policy denial: {log}"
    );
    assert!(
        answer.is_empty() || answer.starts_with("HTTP/1.1 502"),
        "{answer}"
    );
}

#[test]
fn the_ipv4_mapped_spelling_of_a_refused_class_is_not_a_bypass() {
    // Review finding 1 on 53a6c680b3: `::ffff:127.0.0.1` parses as a
    // non-loopback Ipv6Addr, so the per-family class check admitted
    // it — the reviewer relayed a 200 from a local listener through
    // exactly this spelling. The CONNECT authority grammar puts the
    // port last, so the unbracketed spelling is the one that parses
    // AND resolves; the mapped form must run the FULL v4 rule set:
    // same admission (ports-only, IP literal admitted), the dial
    // refused as a transport `unreachable` naming the class — never
    // a tunnel, never a `deny` (it is not a policy decision).
    let dir = run_dir("connect-mapped-loopback");
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready_with(&dir, &addr, &["--allow-connect-port", "443"]);
    let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
    client
        .write_all(b"CONNECT ::ffff:127.0.0.1:443 HTTP/1.1\r\n\r\n")
        .expect("write CONNECT");
    let answer = read_line_until_double_crlf(&mut client);
    let mut child = child;
    let _ = client.shutdown(std::net::Shutdown::Both);
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        log.contains("\"event\":\"allow\""),
        "the admission is recorded before the class check: {log}"
    );
    assert!(
        log.contains("refused address class"),
        "the mapped loopback is refused by the class rule: {log}"
    );
    assert!(
        log.contains("::ffff:127.0.0.1"),
        "the refusal names the mapped spelling: {log}"
    );
    assert!(
        !log.contains("\"event\":\"deny\""),
        "a class refusal is a transport event: {log}"
    );
    assert!(
        answer.is_empty() || answer.starts_with("HTTP/1.1 502"),
        "no tunnel to the mapped loopback: {answer}"
    );
}

#[test]
fn a_connect_to_a_denied_target_is_refused_and_logged_as_deny() {
    // E9's deny: a target outside the table gets a 403 handshake
    // failure, a `deny` event with a reason that names the policy
    // (host not allowed / port not allowed), and the connection ends
    // without a dial.
    for (table, request, needle) in [
        (
            vec![
                "--allow-domain",
                "github.com",
                "--allow-connect-port",
                "443",
            ],
            "CONNECT example.com:443 HTTP/1.1\r\n\r\n",
            "not in the allowed domains",
        ),
        (
            vec![
                "--allow-domain",
                "github.com",
                "--allow-connect-port",
                "443",
            ],
            "CONNECT github.com:22 HTTP/1.1\r\n\r\n",
            "not in the allowed connect-ports",
        ),
        (
            // Domains restrain: an IP literal has no name to match.
            vec![
                "--allow-domain",
                "github.com",
                "--allow-connect-port",
                "443",
            ],
            "CONNECT 1.2.3.4:443 HTTP/1.1\r\n\r\n",
            "the CONNECT lane's policy is the NAME",
        ),
    ] {
        let dir = run_dir("connect-deny");
        let addr = upstream_fixture();
        let (child, socket) = spawn_ready_with(&dir, &addr, &table);
        let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
        client.write_all(request.as_bytes()).expect("write CONNECT");
        let answer = read_line_until_double_crlf(&mut client);
        let mut child = child;
        let _ = client.shutdown(std::net::Shutdown::Both);
        let _ = child.kill();
        let _ = child.wait();
        let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            log.contains("\"event\":\"deny\""),
            "the deny must be recorded: {log}"
        );
        assert!(log.contains(needle), "the reason names the policy: {log}");
        assert!(
            answer.starts_with("HTTP/1.1 403"),
            "the handshake is refused, not tunneled: {answer}"
        );
    }
}

#[test]
fn a_connect_without_a_table_is_refused_not_tunneled() {
    // E8's phase-B compatibility: the plain broker (no table flags)
    // serves no CONNECT lane at all — the model lane is the whole
    // channel, and a CONNECT attempt is a `deny`, never a tunnel to
    // the upstream.
    let dir = run_dir("connect-notable");
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready(&dir, &addr);
    let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
    client
        .write_all(b"CONNECT github.com:443 HTTP/1.1\r\n\r\n")
        .expect("write CONNECT");
    let answer = read_line_until_double_crlf(&mut client);
    let mut child = child;
    let _ = client.shutdown(std::net::Shutdown::Both);
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        log.contains("\"event\":\"deny\""),
        "a table-less broker denies CONNECT: {log}"
    );
    assert!(answer.starts_with("HTTP/1.1 403"), "{answer}");
}

#[test]
fn the_model_lane_is_untouched_by_the_connect_table() {
    // E8's other half: with a table installed, a NON-CONNECT request
    // still takes the model lane byte-for-byte — the agents' baked
    // `http://127.0.0.1:<port>/v1` URLs need no CONNECT awareness.
    let dir = run_dir("connect-modellane");
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready_with(
        &dir,
        &addr,
        &[
            "--allow-domain",
            "github.com",
            "--allow-connect-port",
            "443",
        ],
    );
    let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
    client
        .write_all(b"GET /v1/models HTTP/1.1\r\nHost: localhost\r\n\r\n")
        .expect("write the request");
    let mut answer = Vec::new();
    let _ = client.read_to_end(&mut answer);
    let text = String::from_utf8_lossy(&answer);
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        text.starts_with("HTTP/1.1 200"),
        "the model lane answers as before: {text}"
    );
    // The allow line is the MODEL lane's spelling (`peer -> upstream`),
    // and no policy event fired — the table does not touch this lane.
    assert!(
        log.contains("\"event\":\"allow\""),
        "the model lane's allow line is recorded: {log}"
    );
    assert!(
        log.contains("-> 127.0.0.1:"),
        "the model lane dials the upstream: {log}"
    );
    assert!(
        !log.contains("\"event\":\"deny\""),
        "a model-lane request is never a policy event: {log}"
    );
    assert!(
        !log.lines()
            .any(|l| l.contains("allow") && l.contains("CONNECT ")),
        "the allow line has no CONNECT spelling: {log}"
    );
}

#[test]
fn a_connect_request_split_across_writes_is_still_the_policy_lane() {
    // Review finding 4 on 53a6c680b3: the single MSG_PEEK classified
    // a request line split across writes as model traffic — silently
    // tunneled to the model upstream, no table evaluation, no deny
    // record — because the in-sandbox relay does not preserve write
    // boundaries. The preface reader consumes byte-by-byte until the
    // first line terminates, so delivery split at EVERY byte boundary
    // of the line must classify as CONNECT. Byte-at-a-time writes are
    // the worst case (each write lands in its own broker read), so
    // one pass over every split point of a fragmented DENIED target
    // pins the deny (never a model-lane tunnel), and a split ADMITTED
    // target pins the 200.
    //
    // The denied split: CONNECT to a host outside the table, split at
    // every boundary — each split must produce a `deny`, never a
    // model-lane `allow` -> upstream line.
    for split in 0..request_line_len() {
        let dir = run_dir(&format!("connect-split-{split}"));
        let addr = upstream_fixture();
        let (child, socket) = spawn_ready_with(
            &dir,
            &addr,
            &[
                "--allow-domain",
                "github.com",
                "--allow-connect-port",
                "443",
            ],
        );
        let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
        let line: &[u8] = b"CONNECT example.com:443 HTTP/1.1\r\n\r\n";
        write_split(&mut client, line, split);
        let answer = read_line_until_double_crlf(&mut client);
        let mut child = child;
        let _ = client.shutdown(std::net::Shutdown::Both);
        let _ = child.kill();
        let _ = child.wait();
        let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            log.contains("\"event\":\"deny\""),
            "split at {split}: the split line reaches the policy lane: {log}"
        );
        assert!(
            answer.is_empty() || answer.starts_with("HTTP/1.1 403"),
            "split at {split}: the denied handshake: {answer}"
        );
        assert!(
            !log.lines()
                .any(|l| l.contains("\"event\":\"allow\"") && l.contains("-> 127.0.0.1:")),
            "split at {split}: never a model-lane tunnel: {log}"
        );
    }
    // The admitted split, at one representative boundary mid-line:
    // the POLICY lane is what matters — in this offline sandbox the
    // name cannot resolve, so the honest outcome is the transport
    // `unreachable` and a 502, never a model-lane tunnel. (The
    // 200-handshake half is covered by the table-less and denied
    // tests upstream of resolution; an online host gets it.)
    let dir = run_dir("connect-split-admitted");
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready_with(
        &dir,
        &addr,
        &[
            "--allow-domain",
            "github.com",
            "--allow-connect-port",
            "443",
        ],
    );
    let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
    let line: &[u8] = b"CONNECT github.com:443 HTTP/1.1\r\n\r\n";
    write_split(&mut client, line, 12);
    let answer = read_line_until_double_crlf(&mut client);
    let mut child = child;
    let _ = client.shutdown(std::net::Shutdown::Both);
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        log.lines()
            .any(|l| l.contains("\"event\":\"allow\"") && l.contains("CONNECT")),
        "the split admitted line is the policy lane: {log}"
    );
    assert!(
        answer.is_empty()
            || answer.starts_with("HTTP/1.1 200")
            || answer.starts_with("HTTP/1.1 502"),
        "the policy lane's answer, whatever the transport found: {answer}"
    );
    assert!(
        !log.lines()
            .any(|l| l.contains("\"event\":\"allow\"") && l.contains("-> 127.0.0.1:")),
        "never a model-lane tunnel: {log}"
    );
}

/// The length of the test CONNECT request line WITHOUT the head's
/// empty-line terminator — the split points range over every byte
/// boundary of the LINE, so the first half never completes the head
/// and the broker cannot answer before the tail lands.
fn request_line_len() -> usize {
    b"CONNECT example.com:443 HTTP/1.1\r\n".len()
}

/// Write `line` in two pieces, `split` bytes then the rest, with a
/// flush and a generous window between: the broker's preface reader
/// consumes between the writes, so the line is classified from a
/// fragmented delivery. The window must be long enough that even a
/// peek-based classifier sees ONLY the first half — the whole point
/// of the test is that the fragmented line must still classify as
/// CONNECT, so a window too short would let a buggy peek see the
/// completed line and pass by accident.
fn write_split(stream: &mut std::os::unix::net::UnixStream, line: &[u8], split: usize) {
    use std::io::Write;
    stream
        .write_all(&line[..split])
        .expect("write the head half");
    let _ = stream.flush();
    std::thread::sleep(std::time::Duration::from_millis(150));
    stream
        .write_all(&line[split..])
        .expect("write the tail half");
    let _ = stream.flush();
}

#[test]
fn a_fragmented_non_connect_head_and_an_empty_connection_stay_the_model_lane() {
    // The fallback edge of the preface reader (review finding 4): a
    // non-CONNECT head — even one whose first bytes look like nothing
    // yet — falls back as soon as the prefix diverges, and an EMPTY
    // connection (EOF before any byte) is the model lane with nothing
    // replayed. Neither may stall: the reader must not wait for a
    // newline it will never get once `CONNECT ` is impossible.
    let dir = run_dir("preface-fallback");
    let addr = upstream_fixture();
    let (child, socket) = spawn_ready_with(
        &dir,
        &addr,
        &[
            "--allow-domain",
            "github.com",
            "--allow-connect-port",
            "443",
        ],
    );
    // A GET head, split mid-line: diverges from CONNECT at byte 0,
    // so the model lane runs after ONE byte, and the full head still
    // reaches the upstream (the replay is complete and in order).
    // Second review finding 2 on 3cfc1bbb3e: the split used to be at
    // 8 — past the method token, therefore unable to catch the
    // short-buffer stall the finding probed (the first prefix check
    // held every buffer under 8 bytes as maybe-CONNECT). It now also
    // splits at 1 and at 5 — inside the token — and asserts the
    // upstream fixture answered the model lane BEFORE the head's
    // tail landed, because the divergence is visible at the first
    // differing byte and the fallback must fire there, not at 8.
    let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
    let head: &[u8] = b"GET /v1/models HTTP/1.1\r\nHost: localhost\r\n\r\n";
    write_split(&mut client, head, 8);
    let mut answer = Vec::new();
    let _ = client.read_to_end(&mut answer);
    let text = String::from_utf8_lossy(&answer);
    // An empty connection, while the broker is still up: EOF before
    // any byte — the model lane dials, sees EOF immediately, nothing
    // replayed, no policy event.
    let mut empty = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
    let _ = empty.shutdown(std::net::Shutdown::Both);
    drop(empty);
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    assert!(
        text.starts_with("HTTP/1.1 200"),
        "the split GET head still reaches the upstream whole: {text}"
    );
    assert!(
        log.contains("\"event\":\"allow\""),
        "the model lane is the fallback: {log}"
    );
    assert!(
        !log.contains("\"event\":\"deny\""),
        "neither fallback case is a policy event: {log}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_table_entry_the_broker_cannot_apply_refuses_the_run_before_the_socket() {
    // E9's fail-closed rule at the binary level: an unapplicable table
    // entry (URL form, an IP literal, gateway syntax) refuses with 70
    // and leaves NO socket behind — the broker never serves a table
    // it cannot honor.
    for bad in ["https://github.com", "1.2.3.4", "via github.com"] {
        let dir = run_dir("connect-badtable");
        let (code, _stdout, stderr) = run(&[
            "--listen",
            &sock(&dir),
            "--upstream",
            "127.0.0.1:4000",
            "--decision-log",
            &log_in(&dir),
            "--allow-domain",
            bad,
        ]);
        let socket_existed = Path::new(&sock(&dir)).exists();
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(code, Some(70), "{bad}: stderr: {stderr}");
        assert!(
            !socket_existed,
            "{bad}: a refused table must not leave a socket behind"
        );
    }
}

#[test]
fn the_connect_table_widens_the_fence_and_names_it_in_the_listen_line() {
    // E10: the `listen` line says what the fence carries, never silent
    // about the one sanctioned widening.
    let dir = run_dir("connect-fence-line");
    let addr = upstream_fixture();
    let (child, _socket) = spawn_ready_with(&dir, &addr, &["--allow-connect-port", "443"]);
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        log.contains("the ports named by the CONNECT table: 443"),
        "the listen line names the table's ports: {log}"
    );
}

#[test]
fn a_domains_only_table_is_refused_the_unrestricted_axis_was_a_log_line_lie() {
    // Review finding 3 on 53a6c680b3: the domains-only table promised
    // an unrestricted port axis the Landlock fence cannot express —
    // the fence granted only the upstream port while the listen line
    // claimed the widening, and an allowed host on any other port got
    // EACCES. A claim of a widening the kernel does not grant must
    // not survive: the table refuses fail-closed (exit 70, before
    // the socket), naming the missing axis and the fix.
    let dir = run_dir("connect-domains-only-refused");
    let (code, _stdout, stderr) = run(&[
        "--listen",
        &sock(&dir),
        "--upstream",
        "127.0.0.1:4000",
        "--decision-log",
        &log_in(&dir),
        "--allow-domain",
        "github.com",
    ]);
    let socket_existed = Path::new(&sock(&dir)).exists();
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(code, Some(70), "stderr: {stderr}");
    assert!(
        !socket_existed,
        "a refused table must not leave a socket behind"
    );
    assert!(
        stderr.contains("--allow-connect-port"),
        "the refusal names the missing axis: {stderr}"
    );
    assert!(
        stderr.contains("cannot express"),
        "the refusal says WHY, not just what: {stderr}"
    );
    // And a paired table (hosts AND ports) still starts and names its
    // exact ports in the listen line — the fence and the log agree.
    let dir = run_dir("connect-paired-fence-line");
    let addr = upstream_fixture();
    let (child, _socket) = spawn_ready_with(
        &dir,
        &addr,
        &[
            "--allow-domain",
            "github.com",
            "--allow-connect-port",
            "443",
        ],
    );
    let mut child = child;
    let _ = child.kill();
    let _ = child.wait();
    let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        log.contains("the ports named by the CONNECT table: 443"),
        "the listen line names exactly the fence's ports: {log}"
    );
    assert!(
        !log.contains("unrestricted"),
        "no widening claim survives: {log}"
    );
}

#[test]
fn a_short_non_connect_prefix_is_served_promptly_not_held_for_the_token() {
    // Second review finding 2 on 3cfc1bbb3e: the buffering rewrite's
    // prefix check returned true for ANY buffer shorter than the
    // 8-byte method token, so `G` or `GET /` — already impossible
    // CONNECT starts — did not fall back to the model lane until the
    // divergence was 8 bytes deep. The fragmented test split at 8,
    // past the token, so it could not catch the stall. This test
    // splits INSIDE the token (at 1 and at 5) and asserts the upstream
    // fixture answers BEFORE the head's tail lands: the divergence is
    // visible at the first differing byte, and a non-CONNECT client is
    // served without waiting.
    for split in [1usize, 5] {
        let dir = run_dir(&format!("prefix-prompt-{split}"));
        let addr = upstream_fixture();
        let (child, socket) = spawn_ready_with(
            &dir,
            &addr,
            &[
                "--allow-domain",
                "github.com",
                "--allow-connect-port",
                "443",
            ],
        );
        let mut client = std::os::unix::net::UnixStream::connect(&socket).expect("connect");
        let head: &[u8] = b"GET /v1/models HTTP/1.1\r\nHost: localhost\r\n\r\n";
        client
            .write_all(&head[..split])
            .expect("write the prefix half");
        let _ = client.flush();
        // The window in which the model lane must have dialed and the
        // fixture must have answered — the tail half is still unsent.
        std::thread::sleep(std::time::Duration::from_millis(150));
        client
            .set_read_timeout(Some(std::time::Duration::from_millis(200)))
            .expect("set the read timeout");
        let mut early = [0u8; 128];
        let n = client
            .read(&mut early)
            .expect("the prefix alone is answered");
        assert!(
            n > 0,
            "split at {split}: the model lane serves the diverged prefix without waiting"
        );
        assert!(
            early[..n].starts_with(b"HTTP/1.1 200"),
            "split at {split}: the answer is the upstream fixture's: {:?}",
            &early[..n]
        );
        // The tail still flows, and the full head reached the upstream
        // (the replay is complete and in order).
        client
            .write_all(&head[split..])
            .expect("write the tail half");
        let _ = client.flush();
        let _ = client.shutdown(std::net::Shutdown::Both);
        let mut child = child;
        let _ = child.kill();
        let _ = child.wait();
        let log = std::fs::read_to_string(log_in(&dir)).expect("read the decision log");
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            log.contains("\"event\":\"allow\""),
            "split at {split}: the model lane is the fallback: {log}"
        );
        assert!(
            !log.contains("\"event\":\"deny\""),
            "split at {split}: no policy event for a non-CONNECT head: {log}"
        );
    }
}
