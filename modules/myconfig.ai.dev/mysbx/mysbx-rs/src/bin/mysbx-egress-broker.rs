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
//!    --decision-log <path>` plus the optional CONNECT-lane table,
//!    `--allow-domain <host>` and `--allow-connect-port <port>`
//!    (repeatable; docs/design/egress-connect.md E7–E10, bd
//!    myconfig-ew9s) (`-h`/`--help` prints the usage to stdout and
//!    exits 0; a usage error exits 2).
//! 2. Refuse before doing anything visible (exit 70, one stderr line):
//!    an upstream that is not loopback, a socket path over
//!    `SUN_PATH_LIMIT`, a run directory that is not a real directory
//!    owned by this user with no group/other bits, a kernel whose
//!    Landlock ABI is below 4, a table entry the broker cannot apply.
//! 3. Confine itself with Landlock: `connect(2)` to TCP is allowed on
//!    the fence's ports and nowhere else (E10: the upstream port plus
//!    every `--allow-connect-port`; with domains but no ports the port
//!    axis is unrestricted, the one sanctioned widening, named in the
//!    `listen` line). The fence is applied BEFORE the
//!    socket exists — Landlock does not restrict AF_UNIX, so binding
//!    afterwards is unaffected, and a refused confinement leaves no socket
//!    file behind.
//! 4. Unlink a stale socket at the path (the directory is named after a
//!    run's pid, so whatever sits there belongs to a run that is gone),
//!    bind the listener, and open the decision log — a broker that cannot
//!    record refuses instead of serving silently.
//! 5. Accept connections; per connection, read the first bytes and pick
//!    the lane (E8): a `CONNECT <host>:<port>` request line takes the
//!    policy lane (admit, dial, tunnel — or `deny`), anything else is
//!    the phase-B model lane: dialed straight to the upstream and
//!    copied both ways, the pre-read bytes forwarded first, so agents'
//!    baked `http://127.0.0.1:<port>/v1` URLs need no CONNECT
//!    awareness. One JSON line per decision either way.
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
                     --upstream 127.0.0.1:<port> --decision-log <path> \
                     [--allow-domain <host>]... [--allow-connect-port <port>]...";

/// The parsed invocation.
#[derive(Debug, PartialEq, Eq)]
struct Invocation {
    /// The socket the sandbox reaches (inside the run directory).
    listen: PathBuf,
    /// The one destination the broker dials.
    upstream: SocketAddr,
    /// Where the JSONL decision log is appended.
    decision_log: PathBuf,
    /// The CONNECT lane's `allow-domains` rows (docs/design/
    /// egress-connect.md E9): plain host names, matched exactly,
    /// case-insensitively. Empty means no name may CONNECT.
    allow_domains: Vec<String>,
    /// The CONNECT lane's `connect-ports` rows: the ports a CONNECT
    /// target may name (the port filter of E9's conjunction). Empty
    /// WITH non-empty `allow_domains` means the port axis is
    /// unrestricted (E10's one sanctioned widening).
    allow_connect_ports: Vec<u16>,
}

impl Invocation {
    fn parse(args: &[String]) -> Result<Invocation, String> {
        let mut listen = None;
        let mut upstream = None;
        let mut decision_log = None;
        let mut allow_domains = Vec::new();
        let mut allow_connect_ports = Vec::new();
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
                "--allow-domain" => {
                    let raw = args
                        .get(i + 1)
                        .ok_or_else(|| "`--allow-domain` needs a host name".to_owned())?;
                    allow_domains.push(raw.clone());
                    i += 2;
                }
                "--allow-connect-port" => {
                    let raw = args.get(i + 1).ok_or_else(|| {
                        "`--allow-connect-port` needs a port (1-65535)".to_owned()
                    })?;
                    let port = raw.parse::<u16>().map_err(|_| {
                        format!("`--allow-connect-port {raw}` is not a port (1-65535)")
                    })?;
                    if port == 0 {
                        return Err(format!(
                            "`--allow-connect-port {raw}` is not a port (1-65535)"
                        ));
                    }
                    allow_connect_ports.push(port);
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
            allow_domains,
            allow_connect_ports,
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
    // The CONNECT table is applied, never accepted-and-ignored (E9): a
    // name the broker cannot match, or a table that names nothing, is
    // a refused run. `--allow-connect-port` alone (nono's ports-only
    // semantics, honestly mapped) is a table — any host on those
    // ports; no rows at all is the plain phase-B broker.
    let table = ConnectTable::validate(&inv.allow_domains, &inv.allow_connect_ports)?;
    check_socket_path(&inv.listen)?;
    check_run_dir(&inv.listen)?;

    // The fence is applied BEFORE the socket exists: Landlock restricts
    // only TCP `connect(2)`, so binding the AF_UNIX socket after it is
    // unaffected, and a refused confinement leaves no socket file
    // behind. E10: the upstream port plus every allow-connect-port;
    // with domains but no port rows the port axis is unrestricted (the
    // one sanctioned widening, named in the `listen` line, never
    // silent).
    let mut fence_ports = vec![inv.upstream.port()];
    fence_ports.extend(table.dial_ports());
    confine_to_ports(&fence_ports)?;

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
            "{} -> {}, confined by Landlock to {}",
            inv.listen.display(),
            inv.upstream,
            table.fence_summary()
        ),
    );

    accept_loop(&listener, inv.upstream, &table, &log)
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

/// The CONNECT lane's policy table (docs/design/egress-connect.md
/// E9/E10, bd myconfig-ew9s). Two row classes, one conjunction:
/// `domains` filters the CONNECT target's HOST, `connect_ports` its
/// PORT; both must admit when both are non-empty. `connect_ports`
/// alone means any host on those ports (nono's ports-only semantics,
/// honestly mapped: the empty netns denies every direct connect, so
/// the ports can only mean this lane). `domains` alone means any port
/// on those hosts (E10's sanctioned unrestricted port axis).
///
/// Matching is on the NAME, never an IP literal: the name is the
/// policy, and an IP-literal CONNECT target has no name to match, so
/// it is denied when `domains` is the restricting class (the
/// ports-only table has no names to match and admits any host,
/// IP-literal included, on its allowed ports).
#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ConnectTable {
    /// The allowed host names, lower-cased at validation time.
    domains: Vec<String>,
    /// The allowed target ports, deduplicated, ascending.
    connect_ports: Vec<u16>,
}

impl ConnectTable {
    /// Refuse what the broker cannot apply (E9's fail-closed rule): a
    /// domain that is not a plain host name (URL/path form, an IP
    /// literal, a port suffix), a port of 0, or a table that names
    /// nothing — `--allow-connect-port 0` and friends are refused, not
    /// normalized.
    fn validate(domains: &[String], connect_ports: &[u16]) -> Result<ConnectTable, String> {
        let mut normalized = Vec::new();
        for domain in domains {
            if domain.is_empty() {
                return Err(format!(
                    "`--allow-domain` needs a plain host name; got an empty value"
                ));
            }
            if domain.contains("://")
                || domain.contains('/')
                || domain.contains(':')
                || domain
                    .chars()
                    .any(|c| c.is_whitespace() || c.is_control())
                // A plain host name is letters, digits, hyphens and
                // dots — a wildcard, an arrow or any other sigil is
                // gateway/route syntax, not a name (E11's constraint
                // on the table: it admits targets, never routes).
                || !domain.chars().all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '.')
            {
                return Err(format!(
                    "`--allow-domain {domain}` is not a plain host name — \
                     no scheme, path, port, whitespace or gateway syntax belongs in \
                     it (the broker matches the CONNECT target's host; docs/design/\
                     egress-connect.md E9, E11)"
                ));
            }
            if domain.parse::<std::net::IpAddr>().is_ok() {
                return Err(format!(
                    "`--allow-domain {domain}` is an IP literal; the CONNECT lane's \
                     policy is the NAME, an IP literal has no name to match \
                     (docs/design/egress-connect.md E9)"
                ));
            }
            let lower = domain.to_ascii_lowercase();
            if !normalized.contains(&lower) {
                normalized.push(lower);
            }
        }
        for &port in connect_ports {
            if port == 0 {
                return Err("`--allow-connect-port 0` is not a port (1-65535)".to_owned());
            }
        }
        let mut ports = connect_ports.to_vec();
        ports.sort_unstable();
        ports.dedup();
        Ok(ConnectTable {
            domains: normalized,
            connect_ports: ports,
        })
    }

    /// The ports the Landlock fence must carry beyond the upstream
    /// (E10): the table's port rows. Empty WITH non-empty `domains`
    /// means the port axis is unrestricted — every port must be in the
    /// fence, expressed by the caller NOT narrowing (an unrestricted
    /// axis cannot be listed port by port, so the fence omits the port
    /// rules only when the table's `connect_ports` is empty and its
    /// `domains` is not — see [`Self::fence_summary`]).
    fn dial_ports(&self) -> Vec<u16> {
        if !self.domains.is_empty() && self.connect_ports.is_empty() {
            // Domains-only: every port is dialable for an admitted
            // host, and Landlock grants ports — the fence cannot list
            // them, so it stays at the upstream port plus nothing.
            // The RESOLUTION-side checks (address classes, name
            // matching) carry the host axis; the CONNECT target's
            // port is unrestricted by policy.
            Vec::new()
        } else {
            self.connect_ports.clone()
        }
    }

    /// Whether the CONNECT lane exists at all: no rows, no policy —
    /// the broker is the plain phase-B one and CONNECT is denied as
    /// an unknown request.
    fn is_empty(&self) -> bool {
        self.domains.is_empty() && self.connect_ports.is_empty()
    }

    /// The admission decision for one CONNECT target (E9), the pure
    /// half — resolution happens in [`Self::resolve`] before the
    /// dial, the address classes in [`Self::dialable_addr`].
    ///
    /// Returns `Ok(())` when admitted, `Err(why)` when denied — the
    /// caller writes the `deny` event with `why`.
    fn admit(&self, host: &str, port: u16) -> Result<(), String> {
        // No table, no lane: the phase-B broker serves no CONNECT at
        // all (E8). The caller checks this first and writes its own
        // no-lane refusal; this guard is defense in depth so the
        // pure predicate cannot admit against an empty table.
        if self.is_empty() {
            return Err("the broker serves no CONNECT lane (the table is empty)".to_owned());
        }
        // The port filter applies whenever the table names ports,
        // regardless of the host class (the conjunction).
        if !self.connect_ports.is_empty() && !self.connect_ports.contains(&port) {
            return Err(format!(
                "port {port} is not in the allowed connect-ports ({})",
                self.connect_ports
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
        // The host filter applies when the table names hosts: exact,
        // case-insensitive name match. An IP-literal target is denied
        // — the name is the policy.
        if !self.domains.is_empty() {
            let lower = host.to_ascii_lowercase();
            if host.parse::<std::net::IpAddr>().is_ok() {
                return Err(format!(
                    "an IP-literal CONNECT target ({host}:{port}) is denied — \
                     the CONNECT lane's policy is the NAME (docs/design/
                     egress-connect.md E9)"
                ));
            }
            if !self.domains.contains(&lower) {
                return Err(format!(
                    "host {host} is not in the allowed domains ({})",
                    self.domains.join(", ")
                ));
            }
        }
        Ok(())
    }

    /// Resolve an admitted CONNECT host to ONE address (E9): the
    /// address is chosen here, pinned for the life of the connection,
    /// and returned to be logged. Refuses a name that resolves to
    /// nothing. Resolution is host-side, at connect time — the
    /// rebinding and metadata-range concerns of bd myconfig-fvi
    /// become applicable here, deliberately (see E9).
    fn resolve(&self, host: &str) -> Result<std::net::IpAddr, String> {
        use std::net::ToSocketAddrs;
        let addrs: Vec<std::net::SocketAddr> = (host, 0)
            .to_socket_addrs()
            .map_err(|e| format!("cannot resolve {host}: {e}"))?
            .collect();
        let addr = addrs
            .first()
            .ok_or_else(|| format!("{host} resolves to no address"))?;
        Ok(addr.ip())
    }

    /// The address-class refusal (E9): the CONNECT lane never dials
    /// loopback, link-local, unspecified or multicast targets — only
    /// the model lane's upstream row may reach the host's loopback.
    /// Without this rule an allowed name whose A record points at
    /// `127.0.0.1` or `169.254.169.254` would make the broker a
    /// host-service and metadata client.
    fn dialable_addr(&self, addr: std::net::IpAddr) -> Result<(), String> {
        let refused = match addr {
            std::net::IpAddr::V4(v4) => {
                v4.is_loopback()
                    || v4.is_link_local()
                    || v4.is_unspecified()
                    || v4.is_multicast()
                    || v4.is_broadcast()
                    || v4.is_documentation()
            }
            std::net::IpAddr::V6(v6) => {
                v6.is_loopback() || is_link_local_v6(v6) || v6.is_unspecified() || v6.is_multicast()
            }
        };
        if refused {
            return Err(format!(
                "{addr} is in a refused address class (loopback, link-local, \
                 unspecified or multicast); the CONNECT lane reaches the network, \
                 never the host's own services (docs/design/egress-connect.md E9)"
            ));
        }
        Ok(())
    }

    /// The `listen` line's fence description, never silent about the
    /// one sanctioned widening (E10).
    fn fence_summary(&self) -> String {
        if self.is_empty() {
            "that one port".to_owned()
        } else if !self.domains.is_empty() && self.connect_ports.is_empty() {
            format!(
                "the ports named by the CONNECT table — upstream plus any port \
                 (domains-only: the port axis is unrestricted, the one sanctioned \
                 widening, docs/design/egress-connect.md E10)"
            )
        } else {
            format!(
                "the ports named by the CONNECT table: {}",
                self.connect_ports
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
}

/// `fe80::/10`: `std`'s `Ipv6Addr` has no `is_link_local`.
fn is_link_local_v6(v6: std::net::Ipv6Addr) -> bool {
    (v6.segments()[0] & 0xffc0) == 0xfe80
}

/// Accept connections until the process ends, forwarding each one. A
/// fatal accept error ends the loop as an ERROR: a broker that stopped
/// accepting must not look like a clean exit to the process that spawned
/// it.
fn accept_loop(
    listener: &UnixListener,
    upstream: SocketAddr,
    table: &ConnectTable,
    log: &DecisionLog,
) -> Result<(), String> {
    let table = table.clone();
    loop {
        match listener.accept() {
            Ok((client, _)) => {
                let log = log.clone();
                let table = table.clone();
                // One thread per connection: a slow client must not block
                // the accept loop, an agent may open several.
                std::thread::spawn(move || relay_one(client, upstream, &table, &log));
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

/// Forward one client connection, picking the lane first (E8).
///
/// The first bytes decide: a `CONNECT <host>:<port>` request line
/// takes the policy lane; anything else is the phase-B model lane,
/// forwarded to the upstream with the pre-read bytes first, so a
/// client that speaks plain HTTP to the model endpoint needs no
/// CONNECT awareness and today's behavior is preserved
/// byte-for-byte.
///
/// The events are separate on purpose: `allow` is the admission
/// decision, `deny` is the policy refusal, and
/// `unreachable`/`io`/`resolve` are transport failures AFTER
/// admission. A reader of the log must not mistake a stopped model
/// proxy for a policy refusal.
fn relay_one(client: UnixStream, upstream: SocketAddr, table: &ConnectTable, log: &DecisionLog) {
    let peer = match peer_cred(&client) {
        Some((pid, uid)) => format!("pid {pid} uid {uid}"),
        None => "unknown peer".to_owned(),
    };
    // Read the first bytes without consuming them from the lane's
    // view: a `recv(MSG_PEEK)` keeps the bytes for whichever lane
    // runs, so the model lane can hand the connection over whole and
    // the CONNECT lane can parse its request line off the same
    // stream. (`UnixStream::peek` is unstable in this toolchain; the
    // raw `recv` is the same zero-dependency idiom the crate uses
    // elsewhere.)
    let mut peek = [0u8; 512];
    let n = match recv_peek(&client, &mut peek) {
        Ok(n) => n,
        Err(e) => {
            log.record("io", &format!("{peer}: cannot peek the lane: {e}"));
            return;
        }
    };
    if let Some(target) = parse_connect_request(&peek[..n]) {
        connect_lane(client, target, table, &peer, log);
    } else {
        model_lane(client, upstream, &peer, log);
    }
}

/// Whether the connection's first bytes are a `CONNECT` request line
/// (E8): the one method the policy lane serves. HTTP proxying spells
/// it `CONNECT host:port HTTP/1.1` (RFC 7231 §4.3.6 — the method is
/// case-SENSITIVE, so a lowercase `connect` is not a proxy request);
/// anything else (GET, POST, a bare TLS hello, an empty connection)
/// is the model lane. Returns the `host:port` target of a request
/// line that fits in the peek buffer.
///
/// The line must be TERMINATED inside the peeked bytes: a head that
/// the peek buffer split mid-line is indistinguishable from a
/// complete one, and acting on a truncated head would tunnel a
/// request whose tail is still unread — so a peek without a newline
/// takes the model lane (a real proxy client sends the whole head in
/// one write; a model-lane client whose bytes merely start with the
/// letters `CONNECT` never reaches the policy lane this way).
fn parse_connect_request(bytes: &[u8]) -> Option<ConnectTarget> {
    let text = std::str::from_utf8(bytes).ok()?;
    // The first line, and only if it is terminated inside the peek.
    let newline = text.find('\n')?;
    let line = &text[..newline];
    let line = line.strip_suffix('\r').unwrap_or(line);
    let mut parts = line.split_ascii_whitespace();
    if parts.next()? != "CONNECT" {
        return None;
    }
    let authority = parts.next()?;
    let version = parts.next().unwrap_or("");
    if !version.is_empty() && !version.starts_with("HTTP/") {
        return None;
    }
    // `host:port`, both mandatory — an authority form without a port
    // has no default here (the CONNECT lane grants ports, it does not
    // guess them).
    let (host, port_raw) = authority.rsplit_once(':')?;
    if host.is_empty() {
        return None;
    }
    let port: u16 = port_raw.parse().ok()?;
    if port == 0 {
        return None;
    }
    Some(ConnectTarget {
        host: host.to_owned(),
        port,
    })
}

/// One parsed CONNECT target.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ConnectTarget {
    host: String,
    port: u16,
}

/// The model lane: the phase-B behavior, byte-for-byte. The
/// connection was peeked at, not read, so this dials the upstream
/// and copies both ways as before — the peeked bytes are still in
/// the stream.
fn model_lane(client: UnixStream, upstream: SocketAddr, peer: &str, log: &DecisionLog) {
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
    pipe_bidirectional(client, upstream_stream, peer, log);
}

/// The policy lane (E9): admit, resolve, dial, answer, tunnel. A
/// refused target is a `deny` event and a `403` line back to the
/// client — the CONNECT handshake fails, the client sees a refused
/// tunnel, and the connection ends. An admitted target resolves
/// host-side, checks the address classes, dials, answers `200`, and
/// tunnels.
fn connect_lane(
    mut client: UnixStream,
    target: ConnectTarget,
    table: &ConnectTable,
    peer: &str,
    log: &DecisionLog,
) {
    // No table, no lane: a plain phase-B broker serves no CONNECT at
    // all — the model lane is the whole channel. (This is unreachable
    // when the table is empty because the CLI never emits CONNECT
    // proxy variables then, but a broker whose argv lost its table
    // must still refuse rather than tunnel.)
    if table.is_empty() {
        log.record(
            "deny",
            &format!(
                "{peer} -> {}:{}: the broker serves no CONNECT lane",
                target.host, target.port
            ),
        );
        let _ = client.write_all(b"HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n");
        let _ = client.shutdown(Shutdown::Both);
        return;
    }
    if let Err(why) = table.admit(&target.host, target.port) {
        log.record(
            "deny",
            &format!("{peer} -> {}:{}: {why}", target.host, target.port),
        );
        let _ = client.write_all(b"HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n");
        let _ = client.shutdown(Shutdown::Both);
        return;
    }
    log.record(
        "allow",
        &format!("{peer} CONNECT {}:{}", target.host, target.port),
    );
    // Resolution and address classes are transport steps AFTER the
    // admission decision, so their events are transport events —
    // `unreachable`, never `deny` (a DNS outage must not read as a
    // policy refusal in the log).
    let addr = match table.resolve(&target.host) {
        Ok(addr) => addr,
        Err(e) => {
            log.record(
                "unreachable",
                &format!("{peer} -> {}:{}: {e}", target.host, target.port),
            );
            let _ = client.write_all(b"HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\n\r\n");
            let _ = client.shutdown(Shutdown::Both);
            return;
        }
    };
    if let Err(why) = table.dialable_addr(addr) {
        log.record(
            "unreachable",
            &format!("{peer} -> {}:{} ({addr}): {why}", target.host, target.port),
        );
        let _ = client.write_all(b"HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\n\r\n");
        let _ = client.shutdown(Shutdown::Both);
        return;
    }
    let dialed = std::net::SocketAddr::new(addr, target.port);
    let upstream_stream = match TcpStream::connect(dialed) {
        Ok(stream) => stream,
        Err(e) => {
            log.record(
                "unreachable",
                &format!("{peer} -> {dialed}: cannot connect: {e}"),
            );
            let _ = client.write_all(b"HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\n\r\n");
            let _ = client.shutdown(Shutdown::Both);
            return;
        }
    };
    // The tunnel is up: answer the handshake, then relay both ways.
    // The request line and headers were peeked, not read — consume
    // exactly the request head so the tunneled bytes start at the
    // client's first payload byte.
    read_request_head(&mut client);
    let _ = client.write_all(b"HTTP/1.1 200 Connection Established\r\n\r\n");
    log.record(
        "tunnel",
        &format!("{peer} CONNECT {}:{} -> {dialed}", target.host, target.port),
    );
    pipe_bidirectional(client, upstream_stream, peer, log);
}

/// Read and discard bytes up to (and including) the empty line that
/// ends a request head. `peek` left the head in the stream, so the
/// tunnel must consume it before the first payload byte. Bounded by
/// the peek buffer's worth of reads: a head that never ends leaves
/// the tunnel dead, which the client sees as a refused handshake.
fn read_request_head(client: &mut UnixStream) {
    let mut buf = Vec::new();
    let mut byte = [0u8; 1];
    // The head ends at `\r\n\r\n`; track the last four bytes.
    loop {
        match client.read(&mut byte) {
            Ok(0) => return,
            Ok(_) => {
                buf.push(byte[0]);
                if buf.ends_with(b"\r\n\r\n") || buf.ends_with(b"\n\n") {
                    return;
                }
                if buf.len() > 8 * 1024 {
                    return;
                }
            }
            Err(_) => return,
        }
    }
}

/// Split `client` and `upstream`, then copy both ways with the
/// half-close discipline. The shared tail of both lanes: after the
/// connection is up, the lanes differ only in which stream they
/// dialed.
fn pipe_bidirectional(
    client: UnixStream,
    upstream_stream: TcpStream,
    peer: &str,
    log: &DecisionLog,
) {
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

/// Confine this process to the fence's ports, refusing a kernel that
/// cannot do it. The mechanism is `mysbx::landlock` (the crate's shared
/// binding; `mysbx doctor` reads the same ABI). E10: one rule per
/// port, all in one ruleset.
fn confine_to_ports(ports: &[u16]) -> Result<(), String> {
    let abi = mysbx::landlock::abi().map_err(|e| {
        format!(
            "{e}; the broker confines itself to its TCP ports with Landlock \
             (docs/design/egress.md E5), so a network = \"proxy-only\" run is refused"
        )
    })?;
    if abi < mysbx::landlock::ABI_NET_PORTS {
        return Err(unsupported_landlock(abi));
    }
    mysbx::landlock::confine_to_tcp_ports(ports)
}

/// The effective uid (`geteuid(2)`), the same zero-dependency raw extern
/// idiom the rest of the crate uses.
fn geteuid() -> u32 {
    extern "C" {
        fn geteuid() -> u32;
    }
    unsafe { geteuid() }
}

/// `recv(2)` with `MSG_PEEK`: read up to `buf.len()` bytes WITHOUT
/// consuming them, so the lane decision leaves the stream whole for
/// whichever lane runs. `MSG_PEEK` is `2`; a return of 0 is a closed
/// peer (an empty peek, not an error — the model lane dials anyway and
/// the copy sees EOF immediately).
fn recv_peek(socket: &UnixStream, buf: &mut [u8]) -> Result<usize, io::Error> {
    use std::os::unix::io::AsRawFd;
    extern "C" {
        fn recv(fd: i32, buf: *mut u8, len: usize, flags: i32) -> isize;
    }
    const MSG_PEEK: i32 = 2;
    let rc = unsafe { recv(socket.as_raw_fd(), buf.as_mut_ptr(), buf.len(), MSG_PEEK) };
    if rc < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(rc as usize)
    }
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
        // No table flags: the plain phase-B broker.
        assert!(inv.allow_domains.is_empty());
        assert!(inv.allow_connect_ports.is_empty());
    }

    #[test]
    fn parses_the_connect_table_flags_repeatably() {
        let inv = Invocation::parse(&s(&[
            "--listen",
            "/x/litellm.sock",
            "--upstream",
            "127.0.0.1:4000",
            "--decision-log",
            "/x/log",
            "--allow-domain",
            "github.com",
            "--allow-domain",
            "proxy.internal",
            "--allow-connect-port",
            "443",
            "--allow-connect-port",
            "22",
        ]))
        .unwrap();
        assert_eq!(inv.allow_domains, ["github.com", "proxy.internal"]);
        assert_eq!(inv.allow_connect_ports, [443, 22]);
    }

    #[test]
    fn a_bad_table_value_is_a_usage_error() {
        for args in [
            vec!["--allow-domain"],
            vec!["--allow-connect-port"],
            vec!["--allow-connect-port", "0"],
            vec!["--allow-connect-port", "nope"],
            vec!["--allow-connect-port", "99999"],
        ] {
            let err = Invocation::parse(&s(&args)).unwrap_err();
            assert!(!err.is_empty(), "{args:?}");
            assert_ne!(err, HELP_REQUESTED, "{args:?}");
        }
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

    // ---- The CONNECT table (docs/design/egress-connect.md E9) ----

    #[test]
    fn the_table_refuses_what_it_cannot_apply() {
        // URL and path forms (nono's refused forms, refused here too),
        // port spellings, IP literals — the broker never normalizes a
        // policy entry into something weaker.
        for bad in [
            "https://github.com",
            "github.com/path",
            "github.com:443",
            "1.2.3.4",
            "::1",
            "",
        ] {
            let err = ConnectTable::validate(&[bad.to_owned()], &[]).unwrap_err();
            assert!(
                err.contains("--allow-domain") || err.contains("empty"),
                "{bad}: {err}"
            );
        }
        // A port of 0 in the table is a refusal, not a normalization.
        assert!(ConnectTable::validate(&[], &[0]).is_err());
    }

    #[test]
    fn the_table_normalizes_case_and_deduplicates() {
        let table = ConnectTable::validate(
            &["GitHub.COM".to_owned(), "github.com".to_owned()],
            &[443, 443],
        )
        .unwrap();
        assert_eq!(table.domains, ["github.com"]);
        assert_eq!(table.connect_ports, [443]);
    }

    #[test]
    fn admit_enforces_the_conjunction() {
        let both = ConnectTable::validate(&["github.com".to_owned()], &[443]).unwrap();
        // Admitted: the named host on the named port.
        assert!(both.admit("github.com", 443).is_ok());
        // Denied: the right host, wrong port; the wrong host, right
        // port; the wrong host, wrong port.
        assert!(both.admit("github.com", 22).is_err());
        assert!(both.admit("example.com", 443).is_err());
        assert!(both.admit("example.com", 22).is_err());
        // Case-insensitive host match.
        assert!(both.admit("GITHUB.COM", 443).is_ok());
    }

    #[test]
    fn admit_refuses_ip_literals_only_when_domains_restrain() {
        // With domains in the table an IP-literal CONNECT target is
        // denied: the name is the policy (E9).
        let with_domains = ConnectTable::validate(&["github.com".to_owned()], &[]).unwrap();
        assert!(with_domains.admit("140.82.121.4", 443).is_err());
        // Ports-only has no names to match, so any host — IP literal
        // included — is admitted on the allowed port; the address
        // classes are checked at dial time.
        let ports_only = ConnectTable::validate(&[], &[443]).unwrap();
        assert!(ports_only.admit("140.82.121.4", 443).is_ok());
        assert!(ports_only.admit("anything.invalid", 443).is_ok());
        assert!(ports_only.admit("anything.invalid", 80).is_err());
    }

    #[test]
    fn admit_on_an_empty_table_is_the_no_lane_refusal() {
        // The table-pins-nothing broker serves no CONNECT lane at all:
        // every CONNECT is denied, the model lane is the whole
        // channel (E8's phase-B compatibility).
        let empty = ConnectTable::validate(&[], &[]).unwrap();
        assert!(empty.is_empty());
        assert!(empty.admit("github.com", 443).is_err());
    }

    #[test]
    fn dialable_addr_refuses_the_host_service_classes() {
        let table = ConnectTable::validate(&["github.com".to_owned()], &[]).unwrap();
        for refused in [
            "127.0.0.1",
            "127.8.8.8",
            "169.254.169.254",
            "0.0.0.0",
            "224.0.0.1",
            "255.255.255.255",
            "192.0.2.1",
            "::1",
            "fe80::1",
            "::",
            "ff02::1",
        ] {
            let addr: std::net::IpAddr = refused.parse().unwrap();
            assert!(table.dialable_addr(addr).is_err(), "{refused}");
        }
        // A real network address is dialable; private ranges stay
        // allowed (`allow-domains = ["proxy.internal"]` is a
        // documented use).
        for allowed in ["140.82.121.4", "10.0.0.5", "2001:db8::1"] {
            let addr: std::net::IpAddr = allowed.parse().unwrap();
            assert!(table.dialable_addr(addr).is_ok(), "{allowed}");
        }
    }

    #[test]
    fn the_fence_ports_carry_the_table_rows() {
        // Ports-only and the conjunction: the fence carries every port
        // row.
        let ports = ConnectTable::validate(&[], &[443, 22]).unwrap();
        assert_eq!(ports.dial_ports(), [22, 443]);
        // Domains-only: the port axis is unrestricted and the fence
        // carries NO port rows — the one sanctioned widening, named in
        // the `listen` line.
        let domains = ConnectTable::validate(&["github.com".to_owned()], &[]).unwrap();
        assert_eq!(domains.dial_ports(), Vec::<u16>::new());
        assert!(domains.fence_summary().contains("unrestricted"));
        // Both: the conjunction's ports.
        let both = ConnectTable::validate(&["github.com".to_owned()], &[443]).unwrap();
        assert_eq!(both.dial_ports(), [443]);
        // Empty table: the phase-B fence summary, unchanged.
        let empty = ConnectTable::validate(&[], &[]).unwrap();
        assert_eq!(empty.fence_summary(), "that one port");
    }

    // ---- The lane picker (E8) ----

    #[test]
    fn connect_request_lines_are_recognized_and_parsed() {
        for (line, host, port) in [
            ("CONNECT github.com:443 HTTP/1.1\r\n\r\n", "github.com", 443),
            ("CONNECT github.com:443\r\n", "github.com", 443),
            (
                "CONNECT proxy.internal:8443 HTTP/1.1\r\nHost: x\r\n\r\nextra",
                "proxy.internal",
                8443,
            ),
            (
                "CONNECT [2001:db8::1]:443 HTTP/1.1\r\n\r\n",
                "[2001:db8::1]",
                443,
            ),
        ] {
            let target = parse_connect_request(line.as_bytes()).unwrap_or_else(|| panic!("{line}"));
            assert_eq!(target.host, host, "{line}");
            assert_eq!(target.port, port, "{line}");
        }
    }

    #[test]
    fn non_connect_first_lines_take_the_model_lane() {
        for line in [
            "GET /v1/models HTTP/1.1\r\n\r\n",
            "POST /v1/chat/completions HTTP/1.1\r\n\r\n",
            "\u{16}\u{3}\u{1}\u{0}\u{5}", // a TLS ClientHello begins with non-ASCII
            "",                           // an empty connection
            "CONNECTLESS nonsense\r\n\r\n",
            "connect github.com:443 HTTP/1.1\r\n\r\n", // methods are case-SENSITIVE (RFC 7231)
            "CONNECT github.com:0 HTTP/1.1\r\n\r\n",   // port 0 is not a port
            "CONNECT github.com:abc HTTP/1.1\r\n\r\n", // not a number
            "CONNECT :443 HTTP/1.1\r\n\r\n",           // no host
            "CONNECT github.com:443 WEIRD/1.1\r\n\r\n", // not an HTTP version
            "CONNECT github.com:443", // a truncated head: no terminator inside the peek
        ] {
            assert!(
                parse_connect_request(line.as_bytes()).is_none(),
                "took the CONNECT lane: {line:?}"
            );
        }
    }

    // ---- The rotation reasoning (E11) ----
    //
    // The gateway-rotation trap: a payload re-aims an already-admitted
    // socket at a new destination (IP_TRANSPARENT / TCP_REPAIR /
    // IP_FREEBIND) and splices. The fence, not a setsockopt filter, is
    // what blocks it, and the unit tests pin the policy-side half the
    // binary tests cannot reach (the fence-child test in
    // tests/egress_broker.rs asserts the kernel-side half).

    #[test]
    fn a_denied_connect_logs_deny_and_the_reason_names_the_policy() {
        // The deny's `why` must distinguish the two refusals a reader
        // of the log can act on: a name not in the table vs a port not
        // in the table. A rotation attempt that survives the table (an
        // allowed host) is NOT a policy question — it is a transport
        // question, and its events are `unreachable` ones.
        let table = ConnectTable::validate(&["github.com".to_owned()], &[443]).unwrap();
        let err_host = table.admit("example.com", 443).unwrap_err();
        assert!(
            err_host.contains("not in the allowed domains"),
            "{err_host}"
        );
        let err_port = table.admit("github.com", 22).unwrap_err();
        assert!(
            err_port.contains("not in the allowed connect-ports"),
            "{err_port}"
        );
    }

    #[test]
    fn the_table_has_no_via_or_gateway_rows() {
        // The rotation trap restated as a constraint on the table
        // (E11): the CONNECT lane admits a target, never a route. The
        // table's rows are names and ports; a `--allow-domain` value
        // with gateway syntax is a validation refusal, not a row.
        for gatewayish in [
            "via github.com",
            "github.com via example.com",
            "*->github.com",
        ] {
            assert!(
                ConnectTable::validate(&[gatewayish.to_owned()], &[]).is_err(),
                "{gatewayish}"
            );
        }
    }
}
