// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `mysbx-egress-relay` — the in-sandbox half of the bubblewrap
//! `network = "proxy-only"` channel (docs/design/config.md D20, bd
//! myconfig-mo3.2.5).
//!
//! Inside the sandbox the network namespace is empty: the ONLY host
//! channel is the Unix socket of the host's LiteLLM forwarder, bound
//! at a fixed path. Agents, however, keep their baked
//! `http://127.0.0.1:<litellm port>/v1` URLs. This relay closes the
//! gap: it binds `127.0.0.1:<port>` inside the sandbox and forwards
//! every connection to the bound host socket, so a proxy-aware client
//! needs no configuration change.
//!
//! ## Why a dedicated binary instead of `socat`
//!
//! Decision 5 of the plan (myconfig-n8kv): the listener is bound
//! BEFORE the payload starts, so a client that connects immediately
//! cannot race a half-started relay; a bind failure is visible as exit
//! 70 instead of a payload that fails obscurely; and the crate stays
//! dependency-free (std only, no tokio).
//!
//! ## Process model
//!
//! 1. Parse `--listen 127.0.0.1:<port> --connect <uds> -- <cmd...>`.
//! 2. Bind the TCP listener (failure → exit 70 with a message).
//! 3. Spawn `<cmd>`, with its stdin/stdout/stderr inherited.
//! 4. Forward `SIGINT`/`SIGTERM`/`SIGHUP` to the child (so a `^C`
//!    stops the payload, not just the relay).
//! 5. One accept thread: per connection, connect to the Unix socket
//!    and copy both ways until EOF, then half-close the peer.
//! 6. Exit with the child's status; the relay never outlives it.
//!
//! The relay makes no allow/deny decisions of its own: the fence is
//! the empty netns plus the single bound socket. It exists only to
//! hand loopback TCP clients that one channel.

use std::io::{self, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::os::unix::net::UnixStream;
use std::process::{Child, Command, ExitStatus};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

/// Exit code the pipeline uses for a refused/broken configuration
/// (cli.md); a relay that cannot bind reports the same, so the sandbox
/// shows one class of failure.
const EXIT_INFRASTRUCTURE: i32 = 70;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match run(&args) {
        Ok(code) => std::process::exit(code),
        Err(e) => {
            eprintln!("mysbx-egress-relay: {e}");
            std::process::exit(EXIT_INFRASTRUCTURE);
        }
    }
}

/// The parsed invocation: where to listen, what host socket to forward
/// to, and the payload to run while forwarding.
#[derive(Debug, PartialEq, Eq)]
struct Invocation {
    listen: String,
    connect: String,
    payload: Vec<String>,
}

impl Invocation {
    /// Parse `--listen <addr> --connect <path> -- <cmd...>`. The `--`
    /// separator is required so a payload argument can never be
    /// mistaken for a relay flag; `--listen`/`--connect` are required.
    fn parse(args: &[String]) -> Result<Invocation, String> {
        let mut listen = None;
        let mut connect = None;
        let mut i = 0;
        let mut payload = Vec::new();
        while i < args.len() {
            match args[i].as_str() {
                "--" => {
                    payload = args[i + 1..].to_vec();
                    break;
                }
                "--listen" => {
                    listen = Some(
                        args.get(i + 1)
                            .ok_or_else(|| "`--listen` needs a `<host>:<port>` value".to_owned())?
                            .clone(),
                    );
                    i += 2;
                }
                "--connect" => {
                    connect = Some(
                        args.get(i + 1)
                            .ok_or_else(|| "`--connect` needs a Unix-socket path".to_owned())?
                            .clone(),
                    );
                    i += 2;
                }
                "-h" | "--help" => {
                    return Err(USAGE.to_owned());
                }
                other => return Err(format!("unknown argument `{other}`\n{USAGE}")),
            }
        }
        let listen = listen.ok_or_else(|| format!("missing `--listen`\n{USAGE}"))?;
        let connect = connect.ok_or_else(|| format!("missing `--connect`\n{USAGE}"))?;
        if payload.is_empty() {
            return Err(format!("missing the payload after `--`\n{USAGE}"));
        }
        Ok(Invocation {
            listen,
            connect,
            payload,
        })
    }
}

/// The usage text (`-h`/`--help` and every parse error prints it).
const USAGE: &str =
    "usage: mysbx-egress-relay --listen 127.0.0.1:<port> --connect <unix-socket> -- <cmd...>";

fn run(args: &[String]) -> Result<i32, String> {
    let inv = Invocation::parse(args)?;
    // Bind BEFORE spawning: a port already taken (or an unusable
    // address) is a hard failure the operator sees, not a payload that
    // connects to nothing.
    let listener =
        TcpListener::bind(&inv.listen).map_err(|e| format!("cannot bind {}: {e}", inv.listen))?;

    let mut child = Command::new(&inv.payload[0])
        .args(&inv.payload[1..])
        .spawn()
        .map_err(|e| format!("cannot start `{}`: {e}", inv.payload[0]))?;

    // A `^C` (or a supervisor's SIGTERM/SIGHUP) must stop the PAYLOAD,
    // not leave it orphaned while the relay dies: forward the three
    // termination signals to the child. Nothing else is installed.
    forward_signals(&child);

    let stop = Arc::new(AtomicBool::new(false));
    let connect = inv.connect.clone();
    let accept_stop = Arc::clone(&stop);
    listener
        .set_nonblocking(true)
        .map_err(|e| format!("cannot poll {}: {e}", inv.listen))?;
    let acceptor = std::thread::spawn(move || {
        accept_loop(listener, &connect, &accept_stop);
    });

    let status = match wait_child(&mut child) {
        Ok(status) => status,
        Err(e) => {
            stop.store(true, Ordering::SeqCst);
            let _ = acceptor.join();
            return Err(format!("cannot wait for `{}`: {e}", inv.payload[0]));
        }
    };
    stop.store(true, Ordering::SeqCst);
    // The accept loop polls non-blocking with a short sleep; join it so
    // no in-flight copy keeps the process alive past the payload.
    let _ = acceptor.join();
    Ok(exit_code(status))
}

/// Accept connections until the payload exits, forwarding each one.
fn accept_loop(listener: TcpListener, connect: &str, stop: &AtomicBool) {
    while !stop.load(Ordering::SeqCst) {
        match listener.accept() {
            Ok((client, _)) => {
                let connect = connect.to_owned();
                // One thread per connection: a slow client must not
                // block the accept loop (an agent may open several).
                std::thread::spawn(move || relay_one(client, &connect));
            }
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(_) => break,
        }
    }
}

/// Forward one client connection to the Unix socket and back until
/// both directions are done.
///
/// The failure mode is deliberately quiet: a connection that cannot
/// reach the host socket is dropped (the client sees EOF/reset), and a
/// line on stderr names the reason. A `proxy-only` run whose socket is
/// missing should read as "the model endpoint is unreachable", never
/// as a relay crash that takes the payload down with it.
fn relay_one(client: TcpStream, connect: &str) {
    let upstream = match UnixStream::connect(connect) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("mysbx-egress-relay: cannot reach {connect}: {e}");
            let _ = client.shutdown(Shutdown::Both);
            return;
        }
    };
    let (mut client_read, client_write) = match (client.try_clone(), client) {
        (Ok(r), w) => (r, w),
        (Err(e), _) => {
            eprintln!("mysbx-egress-relay: cannot split the client: {e}");
            return;
        }
    };
    let (mut up_read, up_write) = match (upstream.try_clone(), upstream) {
        (Ok(r), w) => (r, w),
        (Err(e), _) => {
            eprintln!("mysbx-egress-relay: cannot split the upstream: {e}");
            return;
        }
    };
    // client -> upstream, and upstream -> client. Each direction
    // half-closes its WRITE side at EOF, so the peer sees end-of-
    // stream (HTTP/1.1 `Connection: close` and a plain `curl` rely on
    // it) while the other direction keeps flowing.
    let c2u = std::thread::spawn(move || {
        let mut up_write = up_write;
        copy(&mut client_read, &mut up_write);
        let _ = up_write.shutdown(Shutdown::Write);
    });
    let mut client_write = client_write;
    copy(&mut up_read, &mut client_write);
    let _ = client_write.shutdown(Shutdown::Write);
    let _ = c2u.join();
}

/// Copy `from` to `to` until EOF. Errors end the direction quietly: a
/// peer that vanished is the normal end of an HTTP connection.
fn copy<R: Read, W: Write>(from: &mut R, to: &mut W) {
    let mut buf = [0u8; 16 * 1024];
    loop {
        match from.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => {
                if to.write_all(&buf[..n]).is_err() {
                    return;
                }
                let _ = to.flush();
            }
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(_) => return,
        }
    }
}

/// Wait for the payload and return its status. A failed `wait` is a
/// real error (the relay can no longer know how the payload ended), so
/// it is propagated and exits 70 — never reported as a successful 0.
fn wait_child(child: &mut Child) -> std::io::Result<ExitStatus> {
    child.wait()
}

/// The process exit code: the child's code, 128+signal when it was
/// signalled (the shell's convention), or 70 for an unknown status.
fn exit_code(status: ExitStatus) -> i32 {
    if let Some(code) = status.code() {
        return code;
    }
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = status.signal() {
            return 128 + sig;
        }
    }
    EXIT_INFRASTRUCTURE
}

/// Forward SIGINT/SIGTERM/SIGHUP to the child. A raw `signal(2)` with
/// a `static` child pid, the same dependency-free approach `main.rs`
/// uses for SIGPIPE.
#[cfg(unix)]
fn forward_signals(child: &Child) {
    use std::sync::atomic::AtomicI32;
    static CHILD_PID: AtomicI32 = AtomicI32::new(0);
    CHILD_PID.store(child.id() as i32, Ordering::SeqCst);

    #[allow(non_snake_case)]
    extern "C" {
        fn signal(signum: i32, handler: usize) -> usize;
    }
    extern "C" fn forward(signum: i32) {
        let pid = CHILD_PID.load(Ordering::SeqCst);
        if pid > 0 {
            #[allow(non_snake_case)]
            extern "C" {
                fn kill(pid: i32, sig: i32) -> i32;
            }
            unsafe {
                kill(pid, signum);
            }
        }
    }
    const SIGHUP: i32 = 1;
    const SIGINT: i32 = 2;
    const SIGTERM: i32 = 15;
    // `forward as usize` is the explicit cast the extern signature
    // wants; `as *const () as usize` is the lint-clean spelling of the
    // same value.
    let handler = forward as *const () as usize;
    for sig in [SIGHUP, SIGINT, SIGTERM] {
        unsafe {
            signal(sig, handler);
        }
    }
}

#[cfg(not(unix))]
fn forward_signals(_child: &Child) {}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(args: &[&str]) -> Vec<String> {
        args.iter().map(|a| (*a).to_string()).collect()
    }

    #[test]
    fn parses_the_full_invocation() {
        let inv = Invocation::parse(&s(&[
            "--listen",
            "127.0.0.1:4000",
            "--connect",
            "/run/mysbx-egress/litellm.sock",
            "--",
            "/bin/bash",
            "-lc",
            "true",
        ]))
        .unwrap();
        assert_eq!(inv.listen, "127.0.0.1:4000");
        assert_eq!(inv.connect, "/run/mysbx-egress/litellm.sock");
        assert_eq!(inv.payload, vec!["/bin/bash", "-lc", "true"]);
    }

    #[test]
    fn refuses_a_missing_separator_or_flag() {
        assert!(Invocation::parse(&s(&["--listen", "127.0.0.1:1"])).is_err());
        assert!(Invocation::parse(&s(&["--listen", "127.0.0.1:1", "--"])).is_err());
        assert!(
            Invocation::parse(&s(&["--listen", "127.0.0.1:1", "--connect", "/x", "--"])).is_err()
        );
        assert!(Invocation::parse(&s(&[
            "--listen",
            "127.0.0.1:1",
            "--connect",
            "/x",
            "--",
            "true"
        ]))
        .is_ok());
    }

    /// A real TCP + Unix round trip: the client writes, the upstream
    /// socket receives, replies, and the client reads it back.
    #[test]
    fn round_trips_tcp_to_unix() {
        let dir = std::env::temp_dir().join(format!("mysbx-relay-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let sock = dir.join("up.sock");
        let _ = std::fs::remove_file(&sock);
        let up = std::os::unix::net::UnixListener::bind(&sock).unwrap();

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr().unwrap();
        listener.set_nonblocking(true).unwrap();
        let stop = Arc::new(AtomicBool::new(false));
        let stop2 = Arc::clone(&stop);
        let sock2 = sock.to_string_lossy().into_owned();
        let handle = std::thread::spawn(move || accept_loop(listener, &sock2, &stop2));

        // Upstream: accept one connection, read a line, echo it back.
        let up_handle = std::thread::spawn(move || {
            let (mut conn, _) = up.accept().unwrap();
            let mut buf = [0u8; 5];
            conn.read_exact(&mut buf).unwrap();
            conn.write_all(&buf).unwrap();
            buf
        });

        let mut client = TcpStream::connect(addr).unwrap();
        client.write_all(b"hello").unwrap();
        let mut got = [0u8; 5];
        client.read_exact(&mut got).unwrap();
        assert_eq!(&got, b"hello");

        let echoed = up_handle.join().unwrap();
        assert_eq!(&echoed, b"hello");
        stop.store(true, Ordering::SeqCst);
        let _ = handle.join();
        let _ = std::fs::remove_file(&sock);
    }

    /// A connection to a missing host socket is dropped, not a panic:
    /// the relay keeps serving and the client sees its stream closed.
    #[test]
    fn a_missing_socket_drops_the_connection_without_panicking() {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr().unwrap();
        let client = TcpStream::connect(addr).unwrap();
        let (server, _) = listener.accept().unwrap();
        relay_one(server, "/nonexistent/mysbx/relay.sock");
        // The client's connection was shut down (a read returns EOF).
        let mut client = client;
        let mut buf = [0u8; 1];
        assert_eq!(client.read(&mut buf).unwrap(), 0);
    }

    #[test]
    fn exit_code_maps_a_plain_status() {
        // A real child that exits 3.
        let status = Command::new("/bin/sh")
            .args(["-c", "exit 3"])
            .status()
            .unwrap();
        assert_eq!(exit_code(status), 3);
    }
}
