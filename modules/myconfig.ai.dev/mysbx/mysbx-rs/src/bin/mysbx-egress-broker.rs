// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `mysbx-egress-broker` — the host-side half of a `network =
//! "proxy-only"` run (docs/design/egress.md, bd myconfig-n8kv.1).
//!
//! Under `proxy-only` the sandbox's network namespace is empty: the ONLY
//! host channel is one Unix socket, bound read-only into the sandbox.
//! This broker is the host half of that channel: one process per run,
//! owned by mysbx, so the channel can be recorded, scoped to the run, and
//! later carry policy for every backend (docs/design/egress.md E1/E3/E5).
//!
//! ## Process model
//!
//! 1. Parse `--listen <uds path> --upstream 127.0.0.1:<port>
//!    --decision-log <path>` (`-h`/`--help` prints the usage to stdout and
//!    exits 0; a usage error exits 2).
//! 2. Refuse before doing anything visible (exit 70, one stderr line):
//!    an upstream that is not loopback, a socket path over
//!    `SUN_PATH_LIMIT`, a run directory that is not a real directory
//!    owned by this user with no group/other bits, a kernel whose
//!    Landlock ABI is below 4.
//! 3. Confine itself with Landlock: `connect(2)` to TCP is allowed on the
//!    one upstream port and nowhere else. The fence is applied BEFORE the
//!    socket exists — Landlock does not restrict AF_UNIX, so binding
//!    afterwards is unaffected, and a refused confinement leaves no socket
//!    file behind.
//! 4. Unlink a stale socket at the path (the directory is named after a
//!    run's pid, so whatever sits there belongs to a run that is gone),
//!    bind the listener, and open the decision log — a broker that cannot
//!    record refuses instead of serving silently.
//! 5. Accept connections; per connection, dial the upstream and copy both
//!    ways, and append one JSON line to the decision log.
//!
//! The broker has no payload and no shutdown condition of its own: it
//! lives exactly as long as the run, ended from outside (E2 — the
//! spawning process pins it with `PR_SET_PDEATHSIG`, or kills it after
//! reaping the backend). A fatal accept error is the one failure it
//! reports itself, as exit 70.
//!
//! ## Why the log is not in the run directory
//!
//! The sandbox binds the run directory READ-ONLY, so anything inside it
//! is readable by the payload. The log goes to the sidecar
//! (`<sidecar>/egress/<pid>/decisions.jsonl`, E4), which the payload never
//! sees, and it is created 0600 (see `DecisionLog::open`).
//!
//! ## Why Landlock and not `IPAddressAllow=`
//!
//! A static unit can ask systemd for a network filter; a per-run process
//! of the invoking user cannot. Landlock's TCP port rules (ABI 4, Linux
//! 6.7) give the same property in-process, and the broker needs no other
//! privilege to apply them.

use std::io::{self, Read, Write};
use std::net::{Shutdown, SocketAddr, TcpStream};
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// The refusal code (cli.md D8), from the crate so the broker and mysbx
/// cannot drift apart.
const EXIT_INFRASTRUCTURE: i32 = mysbx::EXIT_INFRASTRUCTURE;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let inv = match Invocation::parse(&args) {
        Ok(inv) => inv,
        Err(e) if e == HELP_REQUESTED => {
            println!("{USAGE}");
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("mysbx-egress-broker: {e}");
            std::process::exit(2);
        }
    };
    if let Err(e) = run(&inv) {
        eprintln!("mysbx-egress-broker: {e}");
        std::process::exit(EXIT_INFRASTRUCTURE);
    }
}

/// Marker returned by [`Invocation::parse`] for `-h`/`--help` so `main`
/// can print the usage to STDOUT and exit 0 (a help request is not an
/// error); every real parse error exits 2.
const HELP_REQUESTED: &str = "__help__";

/// The usage text (`-h`/`--help` prints it, and the parse errors that are
/// about the SHAPE of the command line append it).
const USAGE: &str = "usage: mysbx-egress-broker --listen <unix-socket> \
                     --upstream 127.0.0.1:<port> --decision-log <path>";

/// The parsed invocation.
#[derive(Debug, PartialEq, Eq)]
struct Invocation {
    /// The socket the sandbox reaches (inside the run directory).
    listen: PathBuf,
    /// The one destination the broker dials.
    upstream: SocketAddr,
    /// Where the JSONL decision log is appended.
    decision_log: PathBuf,
}

impl Invocation {
    fn parse(args: &[String]) -> Result<Invocation, String> {
        let mut listen = None;
        let mut upstream = None;
        let mut decision_log = None;
        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--listen" => {
                    listen =
                        Some(PathBuf::from(args.get(i + 1).ok_or_else(|| {
                            "`--listen` needs a socket path".to_owned()
                        })?));
                    i += 2;
                }
                "--upstream" => {
                    let raw = args
                        .get(i + 1)
                        .ok_or_else(|| "`--upstream` needs a `<host>:<port>` value".to_owned())?;
                    upstream =
                        Some(raw.parse::<SocketAddr>().map_err(|e| {
                            format!("`--upstream {raw}` is not a socket address: {e}")
                        })?);
                    i += 2;
                }
                "--decision-log" => {
                    decision_log = Some(PathBuf::from(
                        args.get(i + 1)
                            .ok_or_else(|| "`--decision-log` needs a path".to_owned())?,
                    ));
                    i += 2;
                }
                "-h" | "--help" => return Err(HELP_REQUESTED.to_owned()),
                other => return Err(format!("unknown argument `{other}`\n{USAGE}")),
            }
        }
        Ok(Invocation {
            listen: listen.ok_or_else(|| format!("missing `--listen`\n{USAGE}"))?,
            upstream: upstream.ok_or_else(|| format!("missing `--upstream`\n{USAGE}"))?,
            decision_log: decision_log
                .ok_or_else(|| format!("missing `--decision-log`\n{USAGE}"))?,
        })
    }
}

/// Everything the broker refuses before it becomes visible, then the
/// fence, the listener, the log and the accept loop.
fn run(inv: &Invocation) -> Result<(), String> {
    if !inv.upstream.ip().is_loopback() {
        return Err(format!(
            "the upstream {} is not a loopback address; a network = \"proxy-only\" \
             channel reaches the host's model proxy on 127.0.0.1 and nothing else, \
             and Landlock grants a PORT rather than an address, so a non-loopback \
             upstream would be dialed (docs/design/egress.md E1)",
            inv.upstream
        ));
    }
    check_socket_path(&inv.listen)?;
    check_run_dir(&inv.listen)?;

    // The fence is applied BEFORE the socket exists: Landlock restricts
    // only TCP `connect(2)`, so binding the AF_UNIX socket after it is
    // unaffected, and a refused confinement leaves no socket file behind.
    let port = inv.upstream.port();
    confine_to_port(port)?;

    // A stale socket in our own (pid-named) directory belongs to a run
    // that is gone: unlinking is always safe here, and a `bind(2)` on a
    // leftover socket fails with EADDRINUSE. A missing file is not an
    // error; a non-socket at the path is left alone so the bind below
    // refuses it visibly.
    remove_stale_socket(&inv.listen);

    let listener = UnixListener::bind(&inv.listen)
        .map_err(|e| format!("cannot bind {}: {e}", inv.listen.display()))?;
    if let Err(e) = listener.set_nonblocking(true) {
        let _ = std::fs::remove_file(&inv.listen);
        return Err(format!(
            "cannot make {} non-blocking: {e}",
            inv.listen.display()
        ));
    }

    // The log is opened before the first accept: a broker that cannot
    // record refuses instead of serving silently.
    let log = match DecisionLog::open(&inv.decision_log) {
        Ok(log) => log,
        Err(e) => {
            let _ = std::fs::remove_file(&inv.listen);
            return Err(e);
        }
    };
    log.record(
        "listen",
        &format!(
            "{} -> {}, confined by Landlock to that one port",
            inv.listen.display(),
            inv.upstream
        ),
    );

    accept_loop(&listener, inv.upstream, &log)
}

/// The socket path must fit `sockaddr_un::sun_path` (107 bytes, the same
/// constant detached runs use). A computed path over the limit is
/// refused with the limit named, never truncated (docs/design/egress.md
/// E4).
fn check_socket_path(path: &Path) -> Result<(), String> {
    use std::os::unix::ffi::OsStrExt;
    let len = path.as_os_str().as_bytes().len();
    if len > mysbx::detach::SUN_PATH_LIMIT {
        return Err(format!(
            "the socket path {} is {len} bytes, but a Unix socket path is limited \
             to {} bytes; shorten $XDG_RUNTIME_DIR or the socket name \
             (docs/design/egress.md E4)",
            path.display(),
            mysbx::detach::SUN_PATH_LIMIT
        ));
    }
    Ok(())
}

/// The run directory is OUR directory (docs/design/egress.md E3): a real
/// directory, owned by this user, with no group or other permission
/// bits. mysbx creates it; the broker refuses to serve from anything
/// else, because the sandbox binds exactly this directory read-only.
fn check_run_dir(socket: &Path) -> Result<(), String> {
    let dir = socket.parent().ok_or_else(|| {
        format!(
            "the socket path {} has no parent directory",
            socket.display()
        )
    })?;
    let meta = std::fs::symlink_metadata(dir).map_err(|e| {
        format!(
            "the run directory {} does not exist ({e}); mysbx creates it before \
             starting the broker (docs/design/egress.md E3)",
            dir.display()
        )
    })?;
    if !meta.file_type().is_dir() {
        return Err(format!(
            "the run directory {} is not a real directory (a symlink or a plain file); \
             the sandbox binds it read-only, so it must not be redirected \
             (docs/design/egress.md E3)",
            dir.display()
        ));
    }
    let uid = meta.uid();
    let euid = geteuid();
    if uid != euid {
        return Err(format!(
            "the run directory {} is owned by uid {uid}, not by the user running the \
             broker (uid {euid}) (docs/design/egress.md E3)",
            dir.display()
        ));
    }
    let mode = meta.permissions().mode();
    if mode & 0o077 != 0 {
        return Err(format!(
            "the run directory {} is mode {:04o}, so group or other has access to it; \
             it must be 0700 (docs/design/egress.md E3)",
            dir.display(),
            mode & 0o7777
        ));
    }
    Ok(())
}

/// Remove a leftover socket at `path`, leaving anything else in place so
/// the bind reports it.
fn remove_stale_socket(path: &Path) {
    use std::os::unix::fs::FileTypeExt;
    match std::fs::symlink_metadata(path) {
        Ok(meta) if meta.file_type().is_socket() => {
            let _ = std::fs::remove_file(path);
        }
        _ => {}
    }
}

/// The refusal for a kernel that cannot confine the broker.
fn unsupported_landlock(abi: i64) -> String {
    format!(
        "this kernel speaks Landlock ABI {abi}, but TCP port rules need ABI \
         {} (Linux 6.7 or newer); the broker cannot confine itself to the model \
         endpoint, so a network = \"proxy-only\" run is refused \
         (docs/design/egress.md E5)",
        mysbx::landlock::ABI_NET_PORTS
    )
}

/// Accept connections until the process ends, forwarding each one. A
/// fatal accept error ends the loop as an ERROR: a broker that stopped
/// accepting must not look like a clean exit to the process that spawned
/// it.
fn accept_loop(
    listener: &UnixListener,
    upstream: SocketAddr,
    log: &DecisionLog,
) -> Result<(), String> {
    loop {
        match listener.accept() {
            Ok((client, _)) => {
                let log = log.clone();
                // One thread per connection: a slow client must not block
                // the accept loop, an agent may open several.
                std::thread::spawn(move || relay_one(client, upstream, &log));
            }
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                std::thread::sleep(Duration::from_millis(50));
            }
            // Transient: the peer aborted before accept finished, or a
            // signal interrupted the syscall.
            Err(ref e)
                if matches!(
                    e.kind(),
                    io::ErrorKind::ConnectionAborted | io::ErrorKind::Interrupted
                ) => {}
            // Descriptor pressure: back off instead of spinning (EMFILE,
            // ENFILE, ENOBUFS, ENOMEM).
            Err(ref e) if is_transient_accept_pressure(e) => {
                std::thread::sleep(Duration::from_millis(100));
            }
            // Fatal: report it and stop accepting rather than serving
            // silently with no record.
            Err(e) => {
                log.record("fatal", &format!("accept failed: {e}"));
                return Err(format!("accept failed: {e}"));
            }
        }
    }
}

/// Whether an accept error is transient resource pressure (EMFILE,
/// ENFILE, ENOBUFS, ENOMEM) where a short back-off beats a dead broker.
fn is_transient_accept_pressure(e: &io::Error) -> bool {
    matches!(e.raw_os_error(), Some(24) | Some(23) | Some(105) | Some(12))
}

/// Forward one client connection to the upstream and back.
///
/// The events are separate on purpose: `allow` is the admission decision,
/// `unreachable`/`io` are transport failures AFTER admission. A reader of
/// the log must not mistake a stopped model proxy for a policy refusal.
fn relay_one(client: UnixStream, upstream: SocketAddr, log: &DecisionLog) {
    let peer = match peer_cred(&client) {
        Some((pid, uid)) => format!("pid {pid} uid {uid}"),
        None => "unknown peer".to_owned(),
    };
    log.record("allow", &format!("{peer} -> {upstream}"));
    let upstream_stream = match TcpStream::connect(upstream) {
        Ok(stream) => stream,
        Err(e) => {
            log.record(
                "unreachable",
                &format!("{peer} -> {upstream}: cannot connect: {e}"),
            );
            let _ = client.shutdown(Shutdown::Both);
            return;
        }
    };
    let (mut client_read, client_write) = match (client.try_clone(), client) {
        (Ok(r), w) => (r, w),
        (Err(e), _) => {
            log.record("io", &format!("{peer}: cannot split the client: {e}"));
            return;
        }
    };
    let (mut up_read, up_write) = match (upstream_stream.try_clone(), upstream_stream) {
        (Ok(r), w) => (r, w),
        (Err(e), _) => {
            log.record("io", &format!("{peer}: cannot split the upstream: {e}"));
            return;
        }
    };
    // Each direction half-closes its WRITE side at EOF, so the peer sees
    // end-of-stream while the other direction keeps flowing.
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

/// The append-only JSONL decision log (docs/design/egress.md E4). Shared
/// between the accept loop and its per-connection threads, so clones
/// share one `File` — every `write_all` of a whole line is one append.
#[derive(Clone)]
struct DecisionLog {
    file: std::sync::Arc<std::fs::File>,
    path: PathBuf,
}

impl DecisionLog {
    /// Open (creating) the log 0600: it names the peers, the run and the
    /// upstream, so nothing else on the host reads it.
    fn open(path: &Path) -> Result<DecisionLog, String> {
        use std::os::unix::fs::OpenOptionsExt;
        let file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .mode(0o600)
            .open(path)
            .map_err(|e| {
                format!(
                    "cannot open the decision log {}: {e}; the broker records every \
                     decision, so it refuses to serve without one (docs/design/egress.md E4)",
                    path.display()
                )
            })?;
        Ok(DecisionLog {
            file: std::sync::Arc::new(file),
            path: path.to_owned(),
        })
    }

    /// Append one decision. A failed write is reported once on stderr and
    /// does not stop the channel — the fence, not the log, is the
    /// guarantee.
    fn record(&self, event: &str, why: &str) {
        let line = log_line(event, why, SystemTime::now(), std::process::id());
        let mut file = &*self.file;
        if let Err(e) = file.write_all(line.as_bytes()).and_then(|()| file.flush()) {
            eprintln!(
                "mysbx-egress-broker: cannot append to {}: {e}",
                self.path.display()
            );
        }
    }
}

/// One JSONL line: the decision, when, which broker process and why.
/// Serialised by hand (std only, no serde) with the fields quoted and
/// escaped, so a path with a quote cannot break the file.
fn log_line(event: &str, why: &str, at: SystemTime, pid: u32) -> String {
    let ts = at
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    format!(
        "{{\"ts\":{ts},\"pid\":{pid},\"event\":\"{}\",\"why\":\"{}\"}}\n",
        escape(event),
        escape(why)
    )
}

/// Escape a string for the JSON subset the log uses: quotes,
/// backslashes and control characters.
fn escape(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    for ch in raw.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out
}

/// Confine this process to the one upstream port, refusing a kernel that
/// cannot do it. The mechanism is `mysbx::landlock` (the crate's shared
/// binding; `mysbx doctor` reads the same ABI).
fn confine_to_port(port: u16) -> Result<(), String> {
    let abi = mysbx::landlock::abi().map_err(|e| {
        format!(
            "{e}; the broker confines itself to one TCP port with Landlock \
             (docs/design/egress.md E5), so a network = \"proxy-only\" run is refused"
        )
    })?;
    if abi < mysbx::landlock::ABI_NET_PORTS {
        return Err(unsupported_landlock(abi));
    }
    mysbx::landlock::confine_to_tcp_port(port)
}

/// The effective uid (`geteuid(2)`), the same zero-dependency raw extern
/// idiom the rest of the crate uses.
fn geteuid() -> u32 {
    extern "C" {
        fn geteuid() -> u32;
    }
    unsafe { geteuid() }
}

/// The credentials of the peer at the other end of `socket`
/// (`SO_PEERCRED`): pid, uid and gid of the process that connected. The
/// kernel fills them in at `connect(2)`, so the payload cannot claim
/// another identity — this is what makes the decision log say WHICH
/// sandbox reached the proxy.
fn peer_cred(socket: &UnixStream) -> Option<(i32, u32)> {
    use std::os::unix::io::AsRawFd;
    /// `SOL_SOCKET`.
    const SOL_SOCKET: i32 = 1;
    /// `SO_PEERCRED`.
    const SO_PEERCRED: i32 = 17;
    #[repr(C)]
    struct UCred {
        pid: i32,
        uid: u32,
        gid: u32,
    }
    extern "C" {
        fn getsockopt(fd: i32, level: i32, optname: i32, optval: *mut u8, optlen: *mut u32) -> i32;
    }
    let mut cred = UCred {
        pid: 0,
        uid: 0,
        gid: 0,
    };
    let mut len = std::mem::size_of::<UCred>() as u32;
    let rc = unsafe {
        getsockopt(
            socket.as_raw_fd(),
            SOL_SOCKET,
            SO_PEERCRED,
            &mut cred as *mut UCred as *mut u8,
            &mut len,
        )
    };
    if rc != 0 || (len as usize) < std::mem::size_of::<UCred>() {
        return None;
    }
    Some((cred.pid, cred.uid))
}

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
            "/run/user/1000/mysbx-egress/42/litellm.sock",
            "--upstream",
            "127.0.0.1:4000",
            "--decision-log",
            "/repo.mysbx/egress/42/decisions.jsonl",
        ]))
        .unwrap();
        assert_eq!(
            inv.listen,
            PathBuf::from("/run/user/1000/mysbx-egress/42/litellm.sock")
        );
        assert_eq!(inv.upstream.port(), 4000);
        assert_eq!(
            inv.decision_log,
            PathBuf::from("/repo.mysbx/egress/42/decisions.jsonl")
        );
    }

    #[test]
    fn help_is_its_own_marker() {
        for flag in ["-h", "--help"] {
            assert_eq!(
                Invocation::parse(&s(&[flag])),
                Err(HELP_REQUESTED.to_owned())
            );
        }
    }

    #[test]
    fn every_missing_value_is_a_usage_error() {
        for args in [
            vec![],
            vec!["--listen", "/x/litellm.sock"],
            vec!["--listen", "/x/litellm.sock", "--upstream", "127.0.0.1:1"],
            vec!["--bogus"],
            vec!["--upstream"],
        ] {
            let err = Invocation::parse(&s(&args)).unwrap_err();
            assert!(!err.is_empty(), "{args:?}");
            assert_ne!(err, HELP_REQUESTED, "{args:?}");
        }
    }

    #[test]
    fn an_upstream_that_is_not_a_socket_address_is_refused() {
        let err = Invocation::parse(&s(&[
            "--listen",
            "/x/litellm.sock",
            "--upstream",
            "127.0.0.1",
            "--decision-log",
            "/x/log",
        ]))
        .unwrap_err();
        assert!(err.contains("not a socket address"), "{err}");
    }

    #[test]
    fn a_path_over_the_sun_len_limit_is_refused_by_name() {
        let long = format!("/run/user/1000/{}", "a".repeat(120));
        let err = check_socket_path(Path::new(&long)).unwrap_err();
        assert!(err.contains("limited to 107 bytes"), "{err}");
        assert!(err.contains("egress.md E4"), "{err}");
        // The boundary itself is fine.
        let exact = format!("{}/litellm.sock", "b".repeat(80));
        assert_eq!(exact.len(), 93);
        assert!(check_socket_path(Path::new(&exact)).is_ok());
    }

    #[test]
    fn the_log_line_is_one_escaped_json_object() {
        let line = log_line(
            "deny",
            "uid 1000 -> 127.0.0.1:4000: a \"quoted\" \\ path",
            UNIX_EPOCH + Duration::from_secs(42),
            7,
        );
        assert!(line.ends_with("}\n"), "{line}");
        assert_eq!(line.matches('\n').count(), 1, "{line}");
        assert!(line.contains("\"ts\":42"), "{line}");
        assert!(line.contains("\"pid\":7"), "{line}");
        assert!(line.contains("\"event\":\"deny\""), "{line}");
        assert!(line.contains("\\\"quoted\\\""), "{line}");
        assert!(line.contains("\\\\ path"), "{line}");
    }

    #[test]
    fn control_characters_are_escaped() {
        assert_eq!(escape("a\nb\tc"), "a\\nb\\tc");
        assert_eq!(escape("\u{1}"), "\\u0001");
    }
}
