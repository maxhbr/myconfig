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
//! The listener is bound BEFORE the payload starts, so a client that
//! connects immediately cannot race a half-started relay; a bind
//! failure is a visible exit code instead of a payload that fails
//! obscurely; and the crate stays dependency-free (std only, no tokio).
//!
//! ## Process model
//!
//! 1. Parse `--listen 127.0.0.1:<port> --connect <uds> -- <cmd...>`
//!    (`--help` prints the usage and exits 0; a usage error exits 2).
//! 2. Do every fallible setup step — bind the TCP listener, make it
//!    non-blocking — BEFORE the spawn (failure → exit 70 with a
//!    message).
//! 3. Block SIGHUP/SIGINT/SIGTERM, install their handlers, spawn
//!    `<cmd>` with its stdin/stdout/stderr inherited, record its pid,
//!    then unblock: a signal arriving anywhere in that window stays
//!    pending and is forwarded once the pid is known. The payload gets
//!    the mask from before the block, so it receives the signals.
//! 4. One accept thread: per connection, connect to the Unix socket
//!    and copy both ways until EOF, then half-close the peer.
//!    Transient accept errors retry, descriptor pressure backs off,
//!    a fatal one is printed once.
//! 5. Exit with the child's status; a failed `wait` is exit 70. The
//!    relay never outlives the payload.
//!
//! Signal-forwarding rule: SIGHUP and SIGTERM are always forwarded to
//! the payload (a supervisor's shutdown must stop it). SIGINT is
//! forwarded only when a process sent it (`kill(2)`, `si_code` =
//! `SI_USER`); a kernel-raised SIGINT (`SI_KERNEL`) is the terminal's
//! ^C, which already reached the payload in the same foreground process
//! group, so forwarding it would signal the payload twice.
//!
//! The relay makes no allow/deny decisions of its own: the fence is
//! the empty netns plus the single bound socket. It exists only to
//! hand loopback TCP clients that one channel.

use std::io::{self, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::os::unix::net::UnixStream;
use std::os::unix::process::CommandExt;
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
    // Argument parsing happens HERE, so the two classes of failure are
    // distinct: a usage error is exit 2 (cli.md D8), `-h`/`--help`
    // prints to stdout and exits 0, and every failure AFTER a valid
    // parse (bind, spawn, wait) is a runtime failure, exit 70.
    let inv = match Invocation::parse(&args) {
        Ok(inv) => inv,
        Err(e) if e == HELP_REQUESTED => {
            println!("{USAGE}");
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("mysbx-egress-relay: {e}");
            std::process::exit(2);
        }
    };
    match run(&inv) {
        Ok(code) => std::process::exit(code),
        Err(e) => {
            eprintln!("mysbx-egress-relay: {e}");
            std::process::exit(EXIT_INFRASTRUCTURE);
        }
    }
}

/// Marker returned by [`Invocation::parse`] for `-h`/`--help` so
/// `main` can print the usage to STDOUT and exit 0 (a help request is
/// not an error); every real parse error exits 2 (cli.md D8).
const HELP_REQUESTED: &str = "__help__";

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
                    return Err(HELP_REQUESTED.to_owned());
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

fn run(inv: &Invocation) -> Result<i32, String> {
    // Every fallible setup step happens BEFORE the payload is spawned: a
    // relay that cannot listen must fail visibly, never start the payload
    // and then die around it.
    let listener =
        TcpListener::bind(&inv.listen).map_err(|e| format!("cannot bind {}: {e}", inv.listen))?;
    listener
        .set_nonblocking(true)
        .map_err(|e| format!("cannot poll {}: {e}", inv.listen))?;

    // A `^C` (or a supervisor's SIGTERM/SIGHUP) must stop the PAYLOAD,
    // not leave it orphaned while the relay dies. The three signals are
    // BLOCKED first, then the handlers are installed, and the block
    // lasts across the spawn+pid-store: a signal arriving anywhere in
    // that window stays pending and is forwarded once the pid is known
    // (with the handlers installed before the block, a signal in between
    // would reach a handler with no pid and be dropped).
    let blocked =
        unsafe { mysbx::sig::set_of(&[mysbx::sig::HUP, mysbx::sig::INT, mysbx::sig::TERM]) };
    let saved = unsafe { mysbx::sig::mask(mysbx::sig::BLOCK, &blocked) };
    forward_signals();

    // `Command::spawn` passes the calling thread's signal mask on to the
    // child, so the payload would start with the three signals blocked
    // and never see them. Restore the mask from before the block in the
    // child, between fork and exec (`pthread_sigmask` is
    // async-signal-safe).
    let child_mask = saved;
    let mut command = Command::new(&inv.payload[0]);
    command.args(&inv.payload[1..]);
    unsafe {
        command.pre_exec(move || {
            mysbx::sig::restore_mask(&child_mask);
            Ok(())
        });
    }
    let child = command
        .spawn()
        .map_err(|e| format!("cannot start `{}`: {e}", inv.payload[0]));
    let mut child = match child {
        Ok(child) => {
            set_child_pid(child.id());
            unsafe { mysbx::sig::restore_mask(&saved) };
            child
        }
        Err(e) => {
            // Unblock before returning so the process is not left with a
            // changed mask.
            unsafe { mysbx::sig::restore_mask(&saved) };
            return Err(e);
        }
    };

    let stop = Arc::new(AtomicBool::new(false));
    let connect = inv.connect.clone();
    let accept_stop = Arc::clone(&stop);
    let acceptor = std::thread::spawn(move || {
        accept_loop(listener, &connect, &accept_stop);
    });

    let status = match wait_child(&mut child) {
        Ok(status) => status,
        Err(e) => {
            stop.store(true, Ordering::SeqCst);
            // The relay can no longer learn how the payload ended, so it
            // must not leave the payload running unattended: kill it, then
            // report the failure (exit 70).
            let _ = child.kill();
            let _ = child.wait();
            let _ = acceptor.join();
            return Err(format!("cannot wait for `{}`: {e}", inv.payload[0]));
        }
    };
    stop.store(true, Ordering::SeqCst);
    // Clear the pid slot: a late signal must not hit a pid the OS may
    // have reused.
    set_child_pid(0);
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
            // Transient: the peer aborted the connection before accept
            // finished, or a signal interrupted the syscall.
            Err(ref e)
                if matches!(
                    e.kind(),
                    io::ErrorKind::ConnectionAborted | io::ErrorKind::Interrupted
                ) => {}
            // Descriptor pressure: back off briefly instead of spinning
            // (EMFILE/ENFILE surface as `Other` with those raw codes).
            Err(ref e) if is_transient_accept_pressure(e) => {
                std::thread::sleep(Duration::from_millis(100));
            }
            // Fatal: report it once, on stderr, and stop accepting
            // (rather than stopping silently).
            Err(e) => {
                eprintln!("mysbx-egress-relay: accept failed: {e}");
                break;
            }
        }
    }
}

/// Whether an accept error is descriptor exhaustion (`EMFILE` on the
/// process, `ENFILE` system-wide), where a short back-off — rather than
/// a hot spin or a dead relay — is the right response.
fn is_transient_accept_pressure(e: &io::Error) -> bool {
    // EMFILE, ENFILE (descriptor exhaustion) and ENOBUFS, ENOMEM (the
    // kernel out of memory for the new socket): all transient under load,
    // so the accept loop backs off and retries rather than dying.
    matches!(e.raw_os_error(), Some(24) | Some(23) | Some(105) | Some(12))
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

/// The payload pid the signal handler forwards to (0 = not spawned
/// yet, so an early signal is a no-op rather than a kill of pid 0 —
/// which would signal the whole process group).
#[cfg(unix)]
static CHILD_PID: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

/// Record the payload pid for the signal handler.
fn set_child_pid(pid: u32) {
    #[cfg(unix)]
    CHILD_PID.store(pid as i32, Ordering::SeqCst);
    #[cfg(not(unix))]
    let _ = pid;
}

/// Install the SIGINT/SIGTERM/SIGHUP handlers (before the spawn) so a
/// `^C` or a supervisor's signal stops the PAYLOAD. The libc binding
/// lives once in `mysbx::sig`.
#[cfg(unix)]
fn forward_signals() {
    unsafe extern "C" fn forward(signum: i32, info: *const u8, _ctx: *mut u8) {
        let pid = CHILD_PID.load(Ordering::SeqCst);
        // A SIGINT the kernel raised is the terminal's ^C to the
        // foreground process group: the payload is in that group and got
        // it too, so forwarding would signal it twice. A SIGINT sent with
        // kill(2) (SI_USER) targets the relay alone and is forwarded. The
        // decision rests on `si_code`, not on process groups, because
        // inside the sandbox's pid namespace the terminal's process
        // groups are not visible.
        if signum == mysbx::sig::INT
            && !info.is_null()
            && unsafe { mysbx::sig::si_code(info) } == mysbx::sig::SI_KERNEL
        {
            return;
        }
        if pid > 0 {
            unsafe {
                mysbx::sig::kill_pid(pid, signum);
            }
        }
    }
    for sig in [mysbx::sig::HUP, mysbx::sig::INT, mysbx::sig::TERM] {
        unsafe {
            mysbx::sig::on_info(sig, forward);
        }
    }
}

#[cfg(not(unix))]
fn forward_signals() {}

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

    #[test]
    fn a_non_numeric_help_is_a_parse_error_but_h_is_the_marker() {
        // `-h`/`--help` yields the marker (main prints usage to stdout
        // and exits 0); anything else unknown is a real error.
        assert_eq!(Invocation::parse(&s(&["-h"])).unwrap_err(), HELP_REQUESTED);
        assert_eq!(
            Invocation::parse(&s(&["--help"])).unwrap_err(),
            HELP_REQUESTED
        );
        assert!(Invocation::parse(&s(&["--bogus"])).is_err());
        assert_ne!(
            Invocation::parse(&s(&["--bogus"])).unwrap_err(),
            HELP_REQUESTED
        );
    }

    #[test]
    fn descriptor_pressure_is_recognised() {
        // EMFILE (24) / ENFILE (23) get a back-off, not a fatal break;
        // other errors do not.
        for transient in [24, 23, 105, 12] {
            // EMFILE, ENFILE, ENOBUFS, ENOMEM.
            assert!(
                is_transient_accept_pressure(&io::Error::from_raw_os_error(transient)),
                "{transient} must back off and retry"
            );
        }
        for fatal in [9, 22] {
            assert!(
                !is_transient_accept_pressure(&io::Error::from_raw_os_error(fatal)),
                "{fatal} must not be treated as transient"
            );
        }
        assert!(!is_transient_accept_pressure(&io::Error::other("x")));
    }
}
