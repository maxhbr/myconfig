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
//!    myconfig-ew9s), `--public` (bd myconfig-fvi: the `network =
//!    "public"` lane — every target admitted, gated by the
//!    private/special-use deny set), and the host-service rows,
//!    `--host-service <name>=127.0.0.1:<port>` (repeatable; bd
//!    myconfig-db56, docs/design/egress-host-services.md E15)
//!    (`-h`/`--help` prints the usage to stdout and exits 0; a usage
//!    error exits 2).
//! 2. Refuse before doing anything visible (exit 70, one stderr line):
//!    an upstream that is not loopback, a socket path over
//!    `SUN_PATH_LIMIT`, a run directory that is not a real directory
//!    owned by this user with no group/other bits, a kernel whose
//!    Landlock ABI is below 4, a table entry the broker cannot apply.
//! 3. Confine itself with Landlock: `connect(2)` to TCP is allowed on
//!    the fence's ports and nowhere else (E10, corrected by review
//!    finding 3: the upstream port plus every `--allow-connect-port`;
//!    a table that names hosts must name ports too, so the fence, the
//!    listen line and the admission rule are exactly equal — the
//!    domains-only "unrestricted port axis" was never expressible in
//!    the fence and is refused). The fence is applied BEFORE the
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
                     [--allow-domain <host>]... [--allow-connect-port <port>]... \
                     [--public] [--host-service <name>=127.0.0.1:<port>]...";

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
    /// target may name (the port filter of E9's conjunction). Both
    /// axes are named whenever hosts are: `validate` refuses a
    /// domains-only table (review finding 3, correcting E10), so the
    /// fence can always list the ports exactly (E10).
    allow_connect_ports: Vec<u16>,
    /// The `network = "public"` lane (bd myconfig-fvi): every
    /// CONNECT target is admitted, gated only by the public lane's
    /// hard-deny set — the host-service classes every lane refuses
    /// plus the private/special-use ranges (RFC1918, CGNAT, 240/4,
    /// ULA, the NAT64/mapped spellings) and the cloud-metadata
    /// endpoints by name. Mutually exclusive with the table: the
    /// lane is either the E9 table or the public one, never a mix
    /// (a mixed argv would make two admission rules the log could
    /// not tell apart).
    public: bool,
    /// The host-service rows (bd myconfig-db56, docs/design/
    /// egress-host-services.md E15): argv-pinned forwards to NAMED
    /// host loopback endpoints. A row is the LiteLLM row's shape — a
    /// literal loopback target the payload can only SELECT by name,
    /// never choose by address — so it is exempt from the CONNECT
    /// lane's address-class refusal exactly as the upstream is.
    host_services: Vec<HostService>,
}

/// One host-service row (bd myconfig-db56, docs/design/
/// egress-host-services.md E15): `name` selects the row from a
/// CONNECT authority; `target` is the literal loopback endpoint the
/// broker dials when the row is selected. The name is the policy —
/// the same rule as `allow-domains` — so no address the payload can
/// spell reaches a row's target except by naming the row.
#[derive(Debug, Clone, PartialEq, Eq)]
struct HostService {
    /// The row's selector, lower-cased at validation. Plain host-name
    /// grammar; `localhost` is refused (inside the sandbox that name
    /// IS the relay's own listener, bound over /etc/hosts).
    name: String,
    /// `127.0.0.1:<port>` (or `[::1]:<port>`): the host service's
    /// loopback endpoint, literal, never resolved.
    target: SocketAddr,
}

impl Invocation {
    fn parse(args: &[String]) -> Result<Invocation, String> {
        let mut listen = None;
        let mut upstream = None;
        let mut decision_log = None;
        let mut allow_domains = Vec::new();
        let mut allow_connect_ports = Vec::new();
        let mut public = false;
        let mut host_services: Vec<HostService> = Vec::new();
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
                "--public" => {
                    public = true;
                    i += 1;
                }
                "--host-service" => {
                    let raw = args.get(i + 1).ok_or_else(|| {
                        "`--host-service` needs a `<name>=127.0.0.1:<port>` value".to_owned()
                    })?;
                    host_services.push(HostService::parse(raw)?);
                    i += 2;
                }
                // The combined `--host-service=<name>=<target>` spelling:
                // the row IS one value, so the one-token form reads
                // better in an argv dump — and the Nix wrapper pins the
                // same spelling mysbx emits.
                other if other.starts_with("--host-service=") => {
                    host_services.push(HostService::parse(
                        other
                            .strip_prefix("--host-service=")
                            .expect("the guard checked the prefix"),
                    )?);
                    i += 1;
                }
                "-h" | "--help" => return Err(HELP_REQUESTED.to_owned()),
                other => return Err(format!("unknown argument `{other}`\n{USAGE}")),
            }
        }
        if public && (!allow_domains.is_empty() || !allow_connect_ports.is_empty()) {
            return Err(
                "`--public` and the CONNECT table (`--allow-domain`/`--allow-connect-port`) \
                 are mutually exclusive: the lane is either the E9 table or the public \
                 deny set, never a mix (bd myconfig-fvi)"
                    .to_owned(),
            );
        }
        Ok(Invocation {
            listen: listen.ok_or_else(|| format!("missing `--listen`\n{USAGE}"))?,
            upstream: upstream.ok_or_else(|| format!("missing `--upstream`\n{USAGE}"))?,
            decision_log: decision_log
                .ok_or_else(|| format!("missing `--decision-log`\n{USAGE}"))?,
            allow_domains,
            allow_connect_ports,
            public,
            host_services,
        })
    }

    /// The fail-closed content rules, applied in [`run`] with exit
    /// 70 (the CONNECT table's class of refusal — the rows are
    /// policy data, not argv syntax): a row the broker cannot apply
    /// refuses the run before the socket, never a normalized argv.
    fn validated_host_services(&self) -> Result<Vec<HostService>, String> {
        HostService::validate(self.host_services.clone())
    }
}

impl HostService {
    /// Parse one `name=target` row value (a usage error, exit 2):
    /// the `=` splits the selector from the literal target, and the
    /// target must PARSE as a socket address — the argv's SHAPE. The
    /// CONTENT rules (loopback-only target, the name grammar) are
    /// [`Self::validate`]'s, refused in `run` with exit 70 like the
    /// CONNECT table's (the rows are policy data, not argv syntax).
    fn parse(raw: &str) -> Result<HostService, String> {
        let (name, target) = raw
            .split_once('=')
            .ok_or_else(|| format!("`--host-service {raw}` needs a `<name>=<target>` value"))?;
        let target = target.parse::<SocketAddr>().map_err(|e| {
            format!("`--host-service {raw}`: the target is not a socket address: {e}")
        })?;
        Ok(HostService {
            name: name.to_ascii_lowercase(),
            target,
        })
    }

    /// The fail-closed content rules (exit 70, before the socket —
    /// the same class as the CONNECT table's `validate`, so a row
    /// the broker cannot apply is a refused run, not a normalized
    /// one): the target must be a non-zero-port loopback endpoint
    /// (the blast-radius fence: the rows reach HOST services,
    /// nothing else), the name must be a plain host name that is
    /// neither `localhost` (inside the sandbox that name IS the
    /// relay's own listener, bound over /etc/hosts — a row under it
    /// would shadow the model lane's endpoint) nor an IP literal
    /// (the name is the policy, an address would make the row an
    /// address grant), and a duplicated name is a refusal, not a
    /// last-wins — two forwards under one name are two policies the
    /// log could not tell apart. Sorted by name so the fence's port
    /// list and the listen block read deterministically.
    fn validate(rows: Vec<HostService>) -> Result<Vec<HostService>, String> {
        let mut seen: Vec<&str> = Vec::new();
        for row in &rows {
            if !row.target.ip().is_loopback() {
                return Err(format!(
                    "`--host-service {}={}`: the target {} is not a loopback address; a host-service \
                     row reaches a host service listening on loopback, nothing else \
                     (docs/design/egress-host-services.md E15)",
                    row.name,
                    row.target,
                    row.target
                ));
            }
            if row.target.port() == 0 {
                return Err(format!(
                    "`--host-service {}={}`: the target port must be 1-65535",
                    row.name, row.target
                ));
            }
            if row.name.is_empty() {
                return Err(
                    "`--host-service` needs a plain host name as the row's selector — the \
                     payload CONNECTs the name to select the forward \
                     (docs/design/egress-host-services.md E15)"
                        .to_owned(),
                );
            }
            if row.name.eq_ignore_ascii_case("localhost") {
                return Err(
                    "`--host-service localhost=…`: `localhost` is the in-sandbox relay's \
                     own name (bound over /etc/hosts), not a host service; name the row \
                     something else (docs/design/egress-host-services.md E15)"
                        .to_owned(),
                );
            }
            // The IP-literal check runs BEFORE the grammar check: an
            // IP literal contains `:` (and `::1` contains nothing
            // else), so the grammar check would eat it with the wrong
            // message.
            if row.name.parse::<std::net::IpAddr>().is_ok() {
                return Err(format!(
                    "`--host-service {}={}`: the name is an IP literal; a row's name is \
                     the selector, an address would make the row an address grant \
                     (docs/design/egress-host-services.md E15)",
                    row.name, row.target
                ));
            }
            if row.name.contains("://")
                || row.name.contains('/')
                || row.name.contains(':')
                || row.name.contains('=')
                || row
                    .name
                    .chars()
                    .any(|c| c.is_whitespace() || c.is_control())
                || !row
                    .name
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '.')
            {
                return Err(format!(
                    "`--host-service {}={}`: the name is not a plain host name — no scheme, \
                     path, port or gateway syntax belongs in it; the payload selects the \
                     row by CONNECTing to the name (docs/design/egress-host-services.md E15)",
                    row.name, row.target
                ));
            }
            if seen.contains(&row.name.as_str()) {
                return Err(format!(
                    "`--host-service` names `{}` twice; a row name selects exactly \
                     one forward (docs/design/egress-host-services.md E15)",
                    row.name
                ));
            }
            seen.push(&row.name);
        }
        let mut rows = rows;
        rows.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(rows)
    }

    /// The row's port in the Landlock fence (E10's rule on the new
    /// rows): the fence carries every row's target port, so the
    /// fence, the listen line and the dial are exactly equal.
    fn port(&self) -> u16 {
        self.target.port()
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
    // The host-service rows' content rules (bd myconfig-db56, docs/
    // design/egress-host-services.md E15), fail-closed HERE — the
    // same class as the CONNECT table's validate below — so a row
    // the broker cannot apply refuses the run (exit 70) before the
    // socket, and the fence/listen/lane code below consumes a row
    // set that is known-valid.
    let host_services = inv.validated_host_services()?;
    // The CONNECT lane's policy is applied, never accepted-and-
    // ignored (E9): a name the broker cannot match, or a table that
    // names nothing, is a refused run. `--allow-connect-port` alone
    // (nono's ports-only semantics, honestly mapped) is a table —
    // any host on those ports; no rows at all is the plain phase-B
    // broker; `--public` (bd myconfig-fvi) is the public lane.
    let table = ConnectTable::validate(&inv.allow_domains, &inv.allow_connect_ports)?;
    let policy = if inv.public {
        Policy::Public
    } else if table.is_empty() {
        Policy::None
    } else {
        Policy::Table(table)
    };
    check_socket_path(&inv.listen)?;
    check_run_dir(&inv.listen)?;

    // The fence is applied BEFORE the socket exists: Landlock restricts
    // only TCP `connect(2)`, so binding the AF_UNIX socket after it is
    // unaffected, and a refused confinement leaves no socket file
    // behind. E10 (corrected by review finding 3): the upstream port
    // plus every allow-connect-port — the table's ports are always
    // named when its hosts are, so the fence is exactly the admission
    // rule's port set, and the listen line says the same thing. The
    // PUBLIC lane names no ports: its rule is the deny set on the
    // resolved address, not a port fence, and the listen line says
    // so (a Landlock port fence cannot express "every port", so
    // none is claimed).
    // The host-service rows' fence (E15, docs/design/
    // egress-host-services.md): every row's target port joins the
    // fence, so the kernel gate is exactly the row set the listen
    // line names — the E10 rule the CONNECT table learned as review
    // finding 3. The rows are loopback-only, so the fence's port is
    // the only axis a row contributes; the addresses are pinned in
    // the argv and never resolved. Applied regardless of the lane:
    // rows and a table can coexist (the rows are forwards, not
    // grants), so every lane's fence grows the row ports.
    let mut fence_ports = vec![inv.upstream.port()];
    fence_ports.extend(policy.dial_ports());
    for row in &host_services {
        fence_ports.push(row.port());
    }
    fence_ports.sort_unstable();
    fence_ports.dedup();
    match &policy {
        // The table lane and the plain broker fence exactly their
        // ports (the admission rule's port set, E10). The PUBLIC
        // lane carries no TCP port fence: see the position note on
        // [`confine_to_ports`] — its rule is the deny set on the
        // resolved address, which a port fence cannot express. The
        // kernel check still runs (E5's fail-closed rule): a broker
        // that cannot prove the Landlock TCP-port capability refuses
        // rather than serve a lane whose only gate is the code.
        Policy::Public => {
            check_landlock_support()?;
        }
        _ => {
            confine_to_ports(&fence_ports)?;
        }
    }

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
            policy.fence_summary()
        ),
    );
    if !host_services.is_empty() {
        log.record(
            "listen",
            &format!(
                "host-service rows: {}",
                host_services
                    .iter()
                    .map(|r| format!("{}={}", r.name, r.target))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        );
    }

    accept_loop(
        &listener,
        inv.upstream,
        &Lanes {
            policy,
            host_services,
        },
        &log,
    )
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
/// E9/E10, bd myconfig-ew9s; E10 corrected by review finding 3). Two
/// row classes, one conjunction: `domains` filters the CONNECT
/// target's HOST, `connect_ports` its PORT; both axes are named
/// whenever hosts are — a domains-only table is REFUSED, because the
/// unrestricted port axis it implied cannot be expressed in the
/// Landlock fence. `connect_ports` alone means any host on those
/// ports (nono's ports-only semantics, honestly mapped: the empty
/// netns denies every direct connect, so the ports can only mean
/// this lane).
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
        // Domains without ports is REFUSED (review finding 3 on
        // 53a6c680b3, correcting E10): the table's two axes are the
        // HOST (`allow-domains`) and the PORT (`connect-ports`), and
        // a table naming only hosts promised an unrestricted port
        // axis the Landlock fence could not express — the fence
        // stayed at the upstream port while `fence_summary` claimed
        // the widening, so an allowed host on any other port was
        // refused by the kernel with EACCES while the log said it was
        // granted. A claim of a widening the kernel does not grant
        // must not survive; requiring both axes keeps the fence, the
        // listen line and the admission rule exactly equal.
        if !normalized.is_empty() && ports.is_empty() {
            return Err(format!(
                "`--allow-domain {}` names the CONNECT target's HOST axis but no \
                 `--allow-connect-port` names its PORT axis — a domains-only table \
                 promised an unrestricted port axis the Landlock fence cannot \
                 express (docs/design/egress-connect.md E10, corrected); name the \
                 ports too (e.g. 443, 80), or use `--allow-connect-port` alone",
                normalized.join(", ")
            ));
        }
        Ok(ConnectTable {
            domains: normalized,
            connect_ports: ports,
        })
    }

    /// The ports the Landlock fence must carry beyond the upstream
    /// (E10): the table's port rows — always named when hosts are
    /// (`validate` refuses a domains-only table, review finding 3),
    /// so the fence lists exactly the admission rule's ports and the
    /// listen line says the same thing.
    fn dial_ports(&self) -> Vec<u16> {
        self.connect_ports.clone()
    }

    /// Whether the CONNECT lane exists at all: no rows, no policy —
    /// the broker is the plain phase-B one and CONNECT is denied as
    /// an unknown request.
    fn is_empty(&self) -> bool {
        self.domains.is_empty() && self.connect_ports.is_empty()
    }

    /// The admission decision for one CONNECT target (E9), the pure
    /// half — resolution happens in [`Self::resolve`] before the
    /// dial, the address classes in [`Self::dialable_addr`]. Kept on
    /// `ConnectTable` as the TABLE lane's rule ([`Policy::Table`]);
    /// the public lane admits unconditionally and lets the dial's
    /// deny set refuse.
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
    /// become applicable here, deliberately (see E9). A thin wrapper
    /// around [`resolve_connect_host`] so the table's own tests can
    /// call it on the table.
    fn resolve(&self, host: &str) -> Result<std::net::IpAddr, String> {
        resolve_connect_host(host)
    }

    /// The address-class refusal (E9): the TABLE lane's gate — the
    /// host-service classes every lane refuses, nothing more (a
    /// private-range NAME stays dialable here:
    /// `allow-domains = ["proxy.internal"]` is a documented use;
    /// the public lane's wider deny set lives on [`Policy`]). The
    /// mapped-spelling rule is the same: `host_service_class` refuses
    /// the v6 spellings of every refused v4 class (review finding 1
    /// on 53a6c680b3).
    fn dialable_addr(&self, addr: std::net::IpAddr) -> Result<(), String> {
        host_service_class(addr)
    }

    /// The `listen` line's fence description: exactly the ports the
    /// fence grants, so the log never claims a widening the kernel
    /// does not grant (review finding 3: the old domains-only arm
    /// claimed an unrestricted port axis while the fence granted only
    /// the upstream port — a lie in a log line, the exact failure
    /// mode this design was written to avoid).
    fn fence_summary(&self) -> String {
        if self.is_empty() {
            "that one port".to_owned()
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

// The ConnectTable impl ends here; `Policy` (below in the file)
// owns the lane-level rules.

/// The CONNECT lane's policy (bd myconfig-fvi): which admission rule
/// the lane runs. `Table` is the E9 conjunction (domains × ports);
/// `Public` is the fvi lane — every target admitted, the public
/// hard-deny set (and the fence's ports) the only rules. The enum
/// keeps the two admission rules from mixing: a mixed argv is refused
/// at parse time, and the log's `deny` lines always name ONE rule.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
enum Policy {
    /// No CONNECT lane: the plain phase-B broker, CONNECT refused as
    /// an unknown request (the model lane is the whole channel).
    #[default]
    None,
    /// The E9 table conjunction.
    Table(ConnectTable),
    /// The `network = "public"` lane (bd myconfig-fvi).
    Public,
}

impl Policy {
    /// Whether a CONNECT request reaches an admission rule at all:
    /// `None` serves no lane (the caller writes its own refusal).
    fn serves_connect(&self) -> bool {
        !matches!(self, Policy::None)
    }

    /// The admission decision for one CONNECT target: the table's
    /// conjunction, or the public lane's admit. The public lane
    /// refuses the cloud-metadata NAMES (bd myconfig-fvi): their IPs
    /// sit in classes the dial refuses everywhere, but the name is
    /// how a resolver the host trusts would hand the endpoint over,
    /// so the NAME is a policy refusal (`deny`), not a transport
    /// one. Everything else is admitted — the deny set gates the
    /// DIAL, so its refusals are `deny` lines naming the range.
    fn admit(&self, host: &str, port: u16) -> Result<(), String> {
        match self {
            Policy::None => {
                Err("the broker serves no CONNECT lane (the table is empty)".to_owned())
            }
            Policy::Public => {
                let lower = host.to_ascii_lowercase();
                if METADATA_ENDPOINT_NAMES.contains(&lower.as_str()) {
                    return Err(format!(
                        "{host} is a cloud-metadata endpoint; the public lane \
                         never reaches the metadata service (bd myconfig-fvi)"
                    ));
                }
                Ok(())
            }
            Policy::Table(table) => table.admit(host, port),
        }
    }

    /// The address-class gate for the DIAL, after resolution (E9;
    /// fvi): every lane refuses the host-service classes; the public
    /// lane refuses the private/special-use ranges and the
    /// cloud-metadata endpoints too — its admit is the deny set, so
    /// an address the set refuses is a POLICY refusal there, logged
    /// as `deny` (in the table lane the same classes are transport
    /// refusals, `unreachable`, because the table never claimed to
    /// classify them).
    fn dialable_addr(&self, addr: std::net::IpAddr) -> Result<(), String> {
        host_service_class(addr)?;
        if matches!(self, Policy::Public) {
            private_or_special_use(addr).map_err(|why| {
                format!(
                    "{addr} {why}; the public lane reaches the public internet, \
                     never the host's LAN, private or metadata ranges \
                     (bd myconfig-fvi)"
                )
            })?;
        }
        Ok(())
    }

    /// The `listen` line's fence description: exactly the ports the
    /// fence grants. The public lane grants EVERY port (it admits
    /// every target), so the Landlock fence is not the public lane's
    /// rule — the deny set is; the line says so instead of claiming a
    /// port fence that is not there.
    fn fence_summary(&self) -> String {
        match self {
            Policy::None => "that one port".to_owned(),
            Policy::Public => "every port (the public lane's rule is the deny set, not a \
                 port fence — the resolved address is gated, bd myconfig-fvi)"
                .to_owned(),
            Policy::Table(table) => table.fence_summary(),
        }
    }

    /// The ports the Landlock fence must carry beyond the upstream
    /// (E10): the table's rows; the public lane names none — its
    /// gate is the deny set, and a port fence would wrongly claim the
    /// table's semantics.
    fn dial_ports(&self) -> Vec<u16> {
        match self {
            Policy::Table(table) => table.dial_ports(),
            _ => Vec::new(),
        }
    }
}

/// Resolve a CONNECT target's host to ONE address, pinned for the
/// life of the connection (E9; the public lane's fvi rule): the
/// broker dials the RESOLVED address, never the name, so no DNS
/// rebinding can widen an admission after the fact.
fn resolve_connect_host(host: &str) -> Result<std::net::IpAddr, String> {
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

/// The cloud-metadata endpoint NAMES (bd myconfig-fvi): the IPs are
/// refused by the link-local class everywhere, but the names are how
/// a resolver hands the endpoint over, so the public lane refuses
/// them by name at admission — a `deny`, not a transport `502`.
const METADATA_ENDPOINT_NAMES: &[&str] = &["metadata.google.internal", "metadata.azure.internal"];

/// The host-service address classes EVERY lane refuses (E9): the
/// broker must never become a host-service or metadata client,
/// whatever the policy — loopback, link-local, unspecified,
/// multicast, broadcast and the documentation ranges, plus the
/// IPv4-mapped spellings of the same (review finding 1 on
/// 53a6c680b3: `std`'s per-family predicates do not see through the
/// mapping).
fn host_service_class(addr: std::net::IpAddr) -> Result<(), String> {
    let refused = match addr {
        std::net::IpAddr::V4(v4) => {
            v4.is_loopback()
                || v4.is_link_local()
                || v4.is_unspecified()
                || v4.is_multicast()
                || v4.is_broadcast()
                || v4.is_documentation()
        }
        std::net::IpAddr::V6(v6) => match v6.to_ipv4_mapped() {
            // The mapped form IS that v4 address; refuse it exactly
            // as the v4 spelling is refused (loopback, link-local,
            // unspecified, multicast, broadcast, documentation).
            Some(v4) => {
                v4.is_loopback()
                    || v4.is_link_local()
                    || v4.is_unspecified()
                    || v4.is_multicast()
                    || v4.is_broadcast()
                    || v4.is_documentation()
            }
            // Genuine v6: the v6 rules only. `to_ipv4()` (the
            // deprecated IPv4-compatible ::a.b.c.d form) is NOT
            // mapped here on purpose: it is unassigned space no
            // real resolver returns, and treating it as v4 would
            // make `::127.0.0.1` a loopback — harmless to refuse,
            // but the mapped form is the one that reaches the
            // kernel's v4 stack and the one the reviewer probed.
            None => {
                v6.is_loopback() || is_link_local_v6(v6) || v6.is_unspecified() || v6.is_multicast()
            }
        },
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

/// The public lane's hard-deny set (bd myconfig-fvi, n8kv idea 10;
/// the ranges of sandboxes/myconfig.ai.microvm network.nix): the
/// private and special-use ranges a `network = "public"` target must
/// never be — RFC1918, CGNAT 100.64/10, the reserved 240/4 block,
/// IPv6 ULA fc00::/7, and the NAT64-embedded IPv4 spellings
/// (64:ff9b::/96 well-known, 64:ff9b:1::/96 the local-translation
/// prefix — its /96 admits, the REST of 64:ff9b:1::/48 does not).
/// The cloud-metadata IPs (169.254.169.254 and friends) are inside
/// the link-local / metadata classes the dial refuses everywhere;
/// their NAMES are refused in [`Policy::admit`]'s public arm.
fn private_or_special_use(addr: std::net::IpAddr) -> Result<(), &'static str> {
    let refused = match addr {
        std::net::IpAddr::V4(v4) => {
            is_rfc1918_v4(v4)
                || (v4.octets()[0] == 100 && (v4.octets()[1] & 0xc0) == 64)
                || (v4.octets()[0] & 0xf0) == 0xf0
        }
        std::net::IpAddr::V6(v6) => match v6.to_ipv4_mapped() {
            // The mapped form runs the FULL v4 rule set, exactly as
            // the host-service classes do.
            Some(v4) => {
                is_rfc1918_v4(v4)
                    || (v4.octets()[0] == 100 && (v4.octets()[1] & 0xc0) == 64)
                    || (v4.octets()[0] & 0xf0) == 0xf0
            }
            None => {
                let seg = v6.segments();
                // ULA fc00::/7.
                if (seg[0] & 0xfe00) == 0xfc00 {
                    return Err("is a unique-local address (ULA fc00::/7)");
                }
                // NAT64 well-known prefix 64:ff9b::/96: the embedded
                // v4 decides (deny the same ranges).
                if let Some(v4) = nat64_embedded_v4(v6) {
                    if is_private_or_special_v4(v4) {
                        return Err("embeds a private or special-use IPv4 in \
                            64:ff9b::/96");
                    }
                    return Ok(());
                }
                // The LOCAL-translation prefix 64:ff9b:1::/96 embeds
                // its IPv4 the same way and is decided by it; the REST
                // of 64:ff9b:1::/48 is not a translation prefix and
                // is unassigned — refused rather than dialed.
                if seg[0] == 0x0064 && seg[1] == 0xff9b && seg[2] == 0x0001 {
                    return match nat64_local_prefix_v4(v6) {
                        Some(v4) if !is_private_or_special_v4(v4) => Ok(()),
                        Some(_) => Err("embeds a private or special-use IPv4 in \
                                64:ff9b:1::/96"),
                        // Inside the /48 but not the /96: unassigned
                        // rest of the local-translation prefix.
                        None => Err("is in 64:ff9b:1::/48 outside its /96 \
                            translation prefix"),
                    };
                }
                // Every other global v6 is public.
                false
            }
        },
    };
    if refused {
        return Err(
            "is in a private or special-use range (RFC1918, CGNAT, 240/4, \
             ULA or an embedded spelling of one)",
        );
    }
    Ok(())
}

/// RFC1918: 10/8, 172.16/12, 192.168/16.
fn is_rfc1918_v4(v4: std::net::Ipv4Addr) -> bool {
    let o = v4.octets();
    o[0] == 10 || (o[0] == 172 && (o[1] & 0xf0) == 16) || (o[0] == 192 && o[1] == 168)
}

/// The v4 rule of [`private_or_special_use`], for the embedded
/// spellings.
fn is_private_or_special_v4(v4: std::net::Ipv4Addr) -> bool {
    is_rfc1918_v4(v4)
        || (v4.octets()[0] == 100 && (v4.octets()[1] & 0xc0) == 64)
        || (v4.octets()[0] & 0xf0) == 0xf0
}

/// The well-known NAT64 prefix 64:ff9b::/96 embeds its IPv4 in the
/// LOW 32 bits: `64:ff9b::a.b.c.d`. Returns the embedded address.
fn nat64_embedded_v4(v6: std::net::Ipv6Addr) -> Option<std::net::Ipv4Addr> {
    let seg = v6.segments();
    if seg[0] == 0x0064 && seg[1] == 0xff9b && seg[2..6].iter().all(|&s| s == 0) {
        return Some(std::net::Ipv4Addr::new(
            (seg[6] >> 8) as u8,
            seg[6] as u8,
            (seg[7] >> 8) as u8,
            seg[7] as u8,
        ));
    }
    None
}

/// The local-translation NAT64 prefix 64:ff9b:1::/96 embeds its
/// IPv4 the same way. Returns the embedded address.
fn nat64_local_prefix_v4(v6: std::net::Ipv6Addr) -> Option<std::net::Ipv4Addr> {
    let seg = v6.segments();
    if seg[0] == 0x0064 && seg[1] == 0xff9b && seg[2] == 0x0001 && seg[3..6].iter().all(|&s| s == 0)
    {
        return Some(std::net::Ipv4Addr::new(
            (seg[6] >> 8) as u8,
            seg[6] as u8,
            (seg[7] >> 8) as u8,
            seg[7] as u8,
        ));
    }
    None
}

/// `fe80::/10`: `std`'s `Ipv6Addr` has no `is_link_local`.
fn is_link_local_v6(v6: std::net::Ipv6Addr) -> bool {
    (v6.segments()[0] & 0xffc0) == 0xfe80
}

/// The lanes a run's broker serves: the CONNECT policy (the table,
/// the public lane, or none) plus the host-service rows (bd
/// myconfig-db56, docs/design/egress-host-services.md E15). Both are
/// per-run argv facts, cloned into every connection thread.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Lanes {
    policy: Policy,
    host_services: Vec<HostService>,
}

impl Lanes {
    /// The row a CONNECT authority's host selects, if any (E15): the
    /// name is the selector, lower-cased the way the rows were — an
    /// IP-literal host never selects a row (a row's name is a name).
    fn row_for(&self, host: &str) -> Option<&HostService> {
        if host.parse::<std::net::IpAddr>().is_ok() {
            return None;
        }
        let lower = host.to_ascii_lowercase();
        self.host_services.iter().find(|row| row.name == lower)
    }
}

/// Accept connections until the process ends, forwarding each one. A
/// fatal accept error ends the loop as an ERROR: a broker that stopped
/// accepting must not look like a clean exit to the process that spawned
/// it.
fn accept_loop(
    listener: &UnixListener,
    upstream: SocketAddr,
    lanes: &Lanes,
    log: &DecisionLog,
) -> Result<(), String> {
    let lanes = lanes.clone();
    loop {
        match listener.accept() {
            Ok((client, _)) => {
                let log = log.clone();
                let lanes = lanes.clone();
                // One thread per connection: a slow client must not block
                // the accept loop, an agent may open several.
                std::thread::spawn(move || relay_one(client, upstream, &lanes, &log));
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

/// Forward one client connection, picking the lane first (E8, and
/// the host-service rows of bd myconfig-db56).
///
/// The first bytes decide: a `CONNECT <host>:<port>` request line
/// takes a CONNECT-shaped lane — the ROW lane when the host names a
/// host-service row (E15: the row's literal loopback target, no
/// resolution, no address classes — the exemption the upstream row
/// earns by being argv-pinned), else the policy lane; anything else
/// is the phase-B model lane, forwarded to the upstream with the
/// pre-read bytes first, so a client that speaks plain HTTP to the
/// model endpoint needs no CONNECT awareness and today's behavior
/// is preserved byte-for-byte.
///
/// The events are separate on purpose: `allow` is the admission
/// decision, `deny` is the policy refusal, and
/// `unreachable`/`io`/`resolve` are transport failures AFTER
/// admission. A reader of the log must not mistake a stopped model
/// proxy for a policy refusal.
fn relay_one(client: UnixStream, upstream: SocketAddr, lanes: &Lanes, log: &DecisionLog) {
    let policy = &lanes.policy;
    let peer = match peer_cred(&client) {
        Some((pid, uid)) => format!("pid {pid} uid {uid}"),
        None => "unknown peer".to_owned(),
    };
    // Bounded preface buffering (review finding 4 on 53a6c680b3,
    // replacing the single MSG_PEEK): the in-sandbox relay is a dumb
    // byte pipe and does NOT preserve write boundaries, so a CONNECT
    // request line can arrive split across reads — the one-peek
    // classifier saw a truncated line, found no newline, and silently
    // forwarded the request to the model upstream with no table
    // evaluation and no deny record. Instead: CONSUME bytes into a
    // bounded preface until the first line is decisively complete,
    // then classify. The consumed bytes are handed to whichever lane
    // runs — written to the upstream first on the model lane, or
    // counted off the request head on the CONNECT lane — so nothing
    // is lost or reordered. A preface that fills the bound with no
    // decisive first line is the model lane (a model client's head
    // can legitimately exceed the bound; a CONNECT line never does).
    match read_preface(&client) {
        Preface::Connect(head_so_far) => {
            let first_line = first_line_of(&head_so_far).to_vec();
            match parse_connect_request(&first_line) {
                Some(target) => {
                    // The row lane is checked FIRST (E15): a row's
                    // name selects the row on every lane — rows are
                    // forwards, not grants, so they serve under the
                    // plain phase-B broker too, and a row name can
                    // never fall through to the table's name matching
                    // (the row IS the named-destination policy for
                    // that name).
                    if let Some(row) = lanes.row_for(&target.host) {
                        host_service_lane(client, row.clone(), target, &peer, log, head_so_far);
                    } else {
                        connect_lane(client, target, policy, &peer, log, head_so_far);
                    }
                }
                None => model_lane(client, upstream, &peer, log, head_so_far),
            }
        }
        Preface::Model(preface) => model_lane(client, upstream, &peer, log, preface),
    }
}

/// The bounded preface buffer's size: a CONNECT request line is a few
/// dozen bytes and every real proxy client sends it in the first
/// write, but the bound is generous so a head with many headers still
/// classifies without falling back on a line the parser can decide.
const PREFACE_BOUND: usize = 8 * 1024;

/// The lane decision's input: the consumed preface bytes. `Connect`
/// carries a head whose FIRST LINE terminated inside the bound — the
/// parser decides the lane on that line alone. `Model` carries
/// whatever was read before the decision to fall back (a bound filled
/// with no newline, a first line that cannot be `CONNECT`, or EOF on
/// an empty or short connection).
enum Preface {
    Connect(Vec<u8>),
    Model(Vec<u8>),
}

/// Read the connection's preface, CONSUMING it, until the first line
/// is decisively complete or the fallback rules fire. A first line
/// that has already diverged from the `CONNECT ` prefix cannot become
/// one, so bytes are not held hostage while a non-CONNECT client
/// stalls: the model lane runs as soon as the divergence is visible.
fn read_preface(client: &UnixStream) -> Preface {
    let mut buf: Vec<u8> = Vec::new();
    let mut byte = [0u8; 1];
    let mut readable = client;
    loop {
        // A first line that can no longer become `CONNECT `: the model
        // lane, with every consumed byte handed over.
        if !could_start_connect(&buf) {
            return Preface::Model(buf);
        }
        // A terminated first line: decisive either way.
        if buf.iter().any(|&b| b == b'\n') {
            return Preface::Connect(buf);
        }
        if buf.len() >= PREFACE_BOUND {
            return Preface::Model(buf);
        }
        match readable.read(&mut byte) {
            Ok(0) => return Preface::Model(buf),
            Ok(_) => buf.push(byte[0]),
            Err(_) => return Preface::Model(buf),
        }
    }
}

/// Whether `buf` could still be the start of a `CONNECT ` request
/// line: the method token is case-SENSITIVE (RFC 7231), so the check
/// is a byte comparison of the prefix so far. An empty buffer could
/// still become anything. Only the bytes that have ARRIVED are
/// compared — second review finding 2 on 3cfc1bbb3e: the first
/// version short-circuited `len < 8` to true, so a one-byte `G`, or
/// `GET /`, was held as a maybe-CONNECT and the model lane did not
/// run until the divergence became 8 bytes deep; the divergence is
/// visible at the FIRST differing byte and the fallback must fire
/// there (the doc contract: "the model lane runs as soon as the
/// divergence is visible").
fn could_start_connect(buf: &[u8]) -> bool {
    const METHOD: &[u8] = b"CONNECT ";
    let n = buf.len().min(METHOD.len());
    buf[..n] == METHOD[..n]
}

/// The bytes of the preface's first line, without its terminator.
fn first_line_of(head: &[u8]) -> &[u8] {
    let end = head
        .iter()
        .position(|&b| b == b'\n')
        .expect("read_preface only returns Connect with a newline");
    // The line WITH its `\n` terminator: `parse_connect_request`
    // requires the terminated line (it finds the newline itself, so
    // a truncated head can never parse).
    &head[..end + 1]
}

/// Whether the connection's first bytes are a `CONNECT` request line
/// (E8): the one method the policy lane serves. HTTP proxying spells
/// it `CONNECT host:port HTTP/1.1` (RFC 7231 §4.3.6 — the method is
/// case-SENSITIVE, so a lowercase `connect` is not a proxy request);
/// anything else (GET, POST, a bare TLS hello, an empty connection)
/// is the model lane. Returns the `host:port` target of a COMPLETE
/// request line.
///
/// The caller hands the FIRST LINE only, already terminated (the
/// preface reader consumes until the newline, review finding 4): the
/// one-peek classifier classified a line split across writes as
/// model traffic and silently tunneled it — no table evaluation, no
/// deny record — because the in-sandbox relay does not preserve
/// write boundaries. A line that parses here decides the policy
/// lane; a line that does not decides the model lane, with the
/// consumed bytes replayed.
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

/// The model lane: the phase-B behavior, byte-for-byte in what the
/// PEER sees. The preface bytes were CONSUMED while classifying
/// (review finding 4), so they are replayed to the upstream FIRST —
/// in their original order — before the two-way copy continues from
/// the stream; to the upstream the connection is indistinguishable
/// from one that was never buffered.
fn model_lane(
    client: UnixStream,
    upstream: SocketAddr,
    peer: &str,
    log: &DecisionLog,
    preface: Vec<u8>,
) {
    log.record("allow", &format!("{peer} -> {upstream}"));
    let mut upstream_stream = match TcpStream::connect(upstream) {
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
    // The preface was CONSUMED while classifying (review finding 4):
    // replay it to the upstream FIRST, in its original order, so the
    // connection is byte-for-byte what a never-buffered one would
    // have been.
    if !preface.is_empty() {
        if let Err(e) = upstream_stream.write_all(&preface) {
            log.record("io", &format!("{peer}: cannot replay the preface: {e}"));
            return;
        }
    }
    pipe_bidirectional(client, upstream_stream, peer, log);
}

/// The host-service row lane (E15, bd myconfig-db56): a CONNECT whose
/// authority names a row dials the row's LITERAL loopback target — the
/// LiteLLM row's shape, selected by name. The row pins the port too:
/// `CONNECT <name>:<wrong port>` is a `deny` naming the row (the
/// conjunction the table lane already applies — a row is a forward to
/// one endpoint, not a name grant for every port). No resolution and
/// no address classes: the target is argv-pinned and loopback-only,
/// exactly the two properties that earn the upstream its E9 exemption.
fn host_service_lane(
    mut client: UnixStream,
    row: HostService,
    target: ConnectTarget,
    peer: &str,
    log: &DecisionLog,
    head_so_far: Vec<u8>,
) {
    if target.port != row.target.port() {
        log.record(
            "deny",
            &format!(
                "{peer} -> {}:{}: the host-service row `{}` forwards to port {}, \
                 its name selects ONE endpoint (docs/design/egress-host-services.md E15)",
                target.host,
                target.port,
                row.name,
                row.target.port()
            ),
        );
        let _ = client.write_all(b"HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n");
        let _ = client.shutdown(Shutdown::Both);
        return;
    }
    log.record(
        "allow",
        &format!(
            "{peer} CONNECT {} (host-service row `{}`)",
            target.host, row.name
        ),
    );
    let upstream_stream = match TcpStream::connect(row.target) {
        Ok(stream) => stream,
        Err(e) => {
            log.record(
                "unreachable",
                &format!(
                    "{peer} -> {} (row `{}`): cannot connect: {e}",
                    row.target, row.name
                ),
            );
            let _ = client.write_all(b"HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\n\r\n");
            let _ = client.shutdown(Shutdown::Both);
            return;
        }
    };
    // The tunnel tail is the CONNECT lane's (review findings 1/4's
    // byte-exact framing): consume the request-head rest, answer
    // `200`, forward any coalesced payload first, then copy both ways.
    tunnel_tail(
        client,
        upstream_stream,
        target,
        row.target,
        peer,
        log,
        &head_so_far,
    );
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
    policy: &Policy,
    peer: &str,
    log: &DecisionLog,
    head_so_far: Vec<u8>,
) {
    // No lane: a plain phase-B broker serves no CONNECT at all — the
    // model lane is the whole channel. (This is unreachable when no
    // lane exists because the CLI never emits CONNECT proxy variables
    // then, but a broker whose argv lost its lane flags must still
    // refuse rather than tunnel.)
    if !policy.serves_connect() {
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
    if let Err(why) = policy.admit(&target.host, target.port) {
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
    // Resolution is a transport step AFTER the admission decision,
    // so its events are transport events — `unreachable`, never
    // `deny` (a DNS outage must not read as a policy refusal in the
    // log). The address classes keep the table lane's split (a
    // refused class is a transport refusal there); under the PUBLIC
    // lane the deny set IS the policy, so a refused range is a
    // `deny` (the admission claim was "every target", the range is
    // the exception, and the log must say which rule refused).
    let addr = match resolve_connect_host(&target.host) {
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
    if let Err(why) = policy.dialable_addr(addr) {
        if matches!(policy, Policy::Public) {
            log.record(
                "deny",
                &format!("{peer} -> {}:{} ({addr}): {why}", target.host, target.port),
            );
            let _ = client.write_all(b"HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n");
        } else {
            log.record(
                "unreachable",
                &format!("{peer} -> {}:{} ({addr}): {why}", target.host, target.port),
            );
            let _ = client.write_all(b"HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\n\r\n");
        }
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
    // The tail (head remainder, the 200, any payload bytes coalesced
    // past the head's end, then the two-way copy) is `tunnel_tail` —
    // second review finding 1 on 3cfc1bbb3e: the rewrite made
    // `read_request_head` DISCARD everything after the head's
    // terminator, silently swallowing a coalesced initial payload, so
    // the byte tail is split out and pinned by a test that drives it
    // with a socketpair (a dialed tunnel is not provable offline: the
    // class check refuses loopback, the only routable range here).
    tunnel_tail(
        client,
        upstream_stream,
        target,
        dialed,
        peer,
        log,
        &head_so_far,
    );
}

/// The CONNECT lane's byte tail, after the dial succeeded (second
/// review finding 1 on 3cfc1bbb3e): consume the REST of the request
/// head — keeping, not discarding, any bytes the client sent PAST the
/// head's empty-line terminator — answer the handshake, forward those
/// bytes upstream FIRST, then relay both ways. Split from
/// `connect_lane` so the exact framing the finding probed is testable
/// without a dialable address class (loopback is refused everywhere,
/// and this is the seam a real host tunnel exercises unchanged).
fn tunnel_tail(
    mut client: UnixStream,
    mut upstream_stream: TcpStream,
    target: ConnectTarget,
    dialed: SocketAddr,
    peer: &str,
    log: &DecisionLog,
    head_so_far: &[u8],
) {
    let leftover = read_request_head(&mut client, head_so_far);
    let _ = client.write_all(b"HTTP/1.1 200 Connection Established\r\n\r\n");
    log.record(
        "tunnel",
        &format!("{peer} CONNECT {}:{} -> {dialed}", target.host, target.port),
    );
    // A proxy client routinely coalesces its head and its first
    // payload bytes into one write, and the in-sandbox relay preserves
    // no write boundaries — so `leftover` is the tunnel's FIRST
    // payload, not garbage: forward it before the two-way copy starts,
    // in its original order, or it is silently lost (the finding's
    // probe: a 200 that looks successful with the initial payload
    // swallowed). The old pre-rewrite peek classifier could not lose
    // these bytes — it never consumed the head from the stream — so
    // the discard was a regression the finding-4 rewrite introduced
    // while fixing the fragmentation misclassification.
    if !leftover.is_empty() {
        if let Err(e) = upstream_stream.write_all(&leftover) {
            log.record(
                "io",
                &format!("{peer}: cannot forward the tunnel's first bytes: {e}"),
            );
            return;
        }
    }
    pipe_bidirectional(client, upstream_stream, peer, log);
}

/// Read the REST of the request head, given the bytes the preface
/// already consumed (review finding 4), and return every byte read
/// PAST the head's empty-line terminator (`\r\n\r\n` or `\n\n`) —
/// the tunnel's first payload bytes when the client coalesced its
/// head and its payload into one write (second review finding 1 on
/// 3cfc1bbb3e: the first version of this function discarded them).
/// The head itself is consumed and dropped: only its policy-relevant
/// part was parsed, and the dialed upstream expects the tunnel to
/// start at the first byte after it. Bounded: a head that never ends
/// leaves the tunnel dead, which the client sees as a refused
/// handshake.
fn read_request_head(client: &mut UnixStream, head_so_far: &[u8]) -> Vec<u8> {
    // The preface may already hold the terminator with a payload
    // behind it — hand that payload over too; discarding it here is
    // the same data loss the finding probed at the read boundary.
    if let Some(suffix) = suffix_after_head(head_so_far) {
        return suffix.to_vec();
    }
    let mut buf = head_so_far.to_vec();
    let mut chunk = [0u8; 256];
    loop {
        match client.read(&mut chunk) {
            Ok(0) => return Vec::new(),
            Ok(n) => {
                buf.extend_from_slice(&chunk[..n]);
                if let Some(suffix) = suffix_after_head(&buf) {
                    return suffix.to_vec();
                }
                if buf.len() > 8 * 1024 {
                    return Vec::new();
                }
            }
            Err(_) => return Vec::new(),
        }
    }
}

/// The bytes AFTER the request head's empty-line terminator —
/// `None` when the buffer does not contain the terminator (the head
/// is still incomplete). The FIRST terminator ends the head: bytes
/// behind it are tunnel payload, whatever they look like.
fn suffix_after_head(buf: &[u8]) -> Option<&[u8]> {
    let lf = buf.windows(2).position(|w| w == b"\n\n").map(|i| i + 2);
    let crlf = buf.windows(4).position(|w| w == b"\r\n\r\n").map(|i| i + 4);
    let end = match (lf, crlf) {
        (Some(a), Some(b)) => Some(a.min(b)),
        (a, b) => a.or(b),
    };
    end.map(|e| &buf[e..])
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
/// The public lane's Landlock position, in one place: the public
/// lane carries NO TCP port fence. Landlock grants ports, never
/// addresses; the public lane's rule is the deny set on the RESOLVED
/// address, checked in code at the dial, and the target ports are
/// not known up front — a fence that enumerated all 65535 ports would
/// grant exactly what the deny set refuses to classify, claiming a
/// kernel-side policy that does not exist. The model lane's upstream
/// port stays fenced as always; the CONNECT lane's dial is gated by
/// the deny set alone, and the listen line says so
/// ([`Policy::fence_summary`]).
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

/// The kernel-capability check every lane runs (E5's fail-closed
/// rule), fence or no fence: a broker on a kernel that cannot prove
/// the Landlock TCP-port capability (ABI 4) refuses the run, because
/// the public lane's only address gate is the code-level deny set
/// and a silent downgrade must not look like a served lane.
fn check_landlock_support() -> Result<(), String> {
    let abi = mysbx::landlock::abi().map_err(|e| {
        format!(
            "{e}; the broker gates its egress lanes with Landlock \
             (docs/design/egress.md E5), so the run is refused"
        )
    })?;
    if abi < mysbx::landlock::ABI_NET_PORTS {
        return Err(unsupported_landlock(abi));
    }
    Ok(())
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
    fn parses_the_public_flag_and_refuses_a_mixed_lane() {
        // bd myconfig-fvi: `--public` is the public lane's argv. It
        // carries no table — a mixed argv would make two admission
        // rules the log could not tell apart, so the parse refuses it.
        let inv = Invocation::parse(&s(&[
            "--listen",
            "/x/litellm.sock",
            "--upstream",
            "127.0.0.1:4000",
            "--decision-log",
            "/x/log",
            "--public",
        ]))
        .unwrap();
        assert!(inv.public);
        assert!(inv.allow_domains.is_empty());
        assert!(inv.allow_connect_ports.is_empty());
        for mixed in [
            vec!["--allow-domain", "github.com"],
            vec!["--allow-connect-port", "443"],
        ] {
            let mut args = s(&[
                "--listen",
                "/x/litellm.sock",
                "--upstream",
                "127.0.0.1:4000",
                "--decision-log",
                "/x/log",
                "--public",
            ]);
            args.extend(mixed.iter().map(|a| a.to_string()));
            let e = Invocation::parse(&args).unwrap_err();
            assert!(e.contains("mutually exclusive"), "{e}");
        }
    }

    // ---- The host-service rows (bd myconfig-db56, docs/design/
    // ---- egress-host-services.md E15) ------------------------------

    #[test]
    fn parses_the_host_service_rows_in_both_spellings() {
        for (args, name, port) in [
            (
                vec!["--host-service", "my-mcp=127.0.0.1:8080"],
                "my-mcp",
                8080,
            ),
            (vec!["--host-service=my-mcp=127.0.0.1:8080"], "my-mcp", 8080),
            (
                // The name is lower-cased at parse, like the table's
                // domains; the [::1] target spelling is legal.
                vec!["--host-service", "My-Mcp=[::1]:9443"],
                "my-mcp",
                9443,
            ),
        ] {
            let mut full = s(&[
                "--listen",
                "/x/litellm.sock",
                "--upstream",
                "127.0.0.1:4000",
                "--decision-log",
                "/x/log",
            ]);
            full.extend(args.iter().map(|a| (*a).to_string()));
            let inv = Invocation::parse(&full).unwrap();
            assert_eq!(inv.host_services.len(), 1);
            assert_eq!(inv.host_services[0].name, name);
            assert_eq!(inv.host_services[0].target.port(), port);
        }
    }

    #[test]
    fn a_row_without_a_target_is_a_usage_error() {
        for bad in [
            vec!["--host-service", "no-target"],
            vec!["--host-service", "x=not-an-addr"],
            vec!["--host-service"],
        ] {
            let mut full = s(&[
                "--listen",
                "/x/litellm.sock",
                "--upstream",
                "127.0.0.1:4000",
                "--decision-log",
                "/x/log",
            ]);
            full.extend(bad.iter().map(|a| (*a).to_string()));
            let err = Invocation::parse(&full).unwrap_err();
            assert!(!err.is_empty(), "{bad:?}");
            assert_ne!(err, HELP_REQUESTED, "{bad:?}");
        }
    }

    #[test]
    fn row_content_rules_are_fail_closed_not_usage_errors() {
        // The content rules live in `validate` (exit 70 in `run`), so
        // PARSE accepts the shape and `validate` refuses the content —
        // the CONNECT table's split: a policy refusal must not look
        // like a CLI typo.
        let row = |raw: &str| HostService::parse(raw).unwrap();
        // A good row passes both halves.
        let good = HostService::validate(vec![row("my-mcp=127.0.0.1:8080")]).unwrap();
        assert_eq!(good.len(), 1);
        // Non-loopback target, `localhost`, an IP literal, gateway
        // syntax, port 0, a duplicate name: all refused by validate.
        for raws in [
            vec!["lan-thing=10.0.0.5:8080"],
            vec!["localhost=127.0.0.1:8080"],
            vec!["127.0.0.1=127.0.0.1:8080"],
            vec!["::1=127.0.0.1:8080"],
            vec!["via x=127.0.0.1:8080"],
            vec!["http://x=127.0.0.1:8080"],
            vec!["a/b=127.0.0.1:8080"],
            vec!["my-mcp=127.0.0.1:0"],
            vec!["my-mcp=127.0.0.1:8080", "my-mcp=127.0.0.1:9090"],
        ] {
            let rows: Vec<HostService> = raws.iter().map(|r| row(r)).collect();
            let err = HostService::validate(rows).unwrap_err();
            assert!(!err.is_empty(), "{raws:?}");
        }
        // The parse half accepts the SHAPE of every one of them —
        // the refusal is the content rule's, exit 70, not 2 (checked
        // end to end by the binary tests).
        assert!(HostService::parse("lan-thing=10.0.0.5:8080").is_ok());
    }

    #[test]
    fn the_lane_picker_selects_rows_by_name_only() {
        // E15: the row's selector is its NAME, lower-cased; an
        // IP-literal CONNECT host never selects a row (a row's name is
        // a name — `row_for` must not fall through to address
        // matching).
        let lanes = Lanes {
            policy: Policy::None,
            host_services: vec![HostService {
                name: "my-mcp".to_owned(),
                target: "127.0.0.1:8080".parse().unwrap(),
            }],
        };
        assert!(lanes.row_for("my-mcp").is_some());
        assert!(lanes.row_for("MY-MCP").is_some());
        assert!(lanes.row_for("127.0.0.1").is_none());
        assert!(lanes.row_for("other.host").is_none());
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
        let with_domains = ConnectTable::validate(&["github.com".to_owned()], &[443]).unwrap();
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
        let table = ConnectTable::validate(&["github.com".to_owned()], &[443]).unwrap();
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
    fn dialable_addr_refuses_the_ipv4_mapped_forms_of_the_refused_classes() {
        // Review finding 1 on 53a6c680b3: `std`'s per-family predicates
        // do not see through the IPv4-mapped form —
        // `'::ffff:127.0.0.1'.parse::<Ipv6Addr>()` is NOT `is_loopback()`
        // — so every refused v4 class had a v6 spelling that passed the
        // class check and dialed the host service it names. The mapped
        // form runs the FULL v4 rule set; genuine v6 keeps the v6 rules.
        let table = ConnectTable::validate(&["github.com".to_owned()], &[443]).unwrap();
        // The mapped spellings of every refused v4 class, including the
        // documentation range the v4 rule already carried.
        for refused in [
            "::ffff:127.0.0.1",
            "::ffff:127.8.8.8",
            "::ffff:169.254.169.254",
            "::ffff:0.0.0.0",
            "::ffff:224.0.0.1",
            "::ffff:255.255.255.255",
            "::ffff:192.0.2.1",
        ] {
            let addr: std::net::IpAddr = refused.parse().unwrap();
            assert!(table.dialable_addr(addr).is_err(), "{refused}");
        }
        // A mapped PRIVATE or real address stays dialable like its v4
        // spelling: the mapping refuses classes, not the address family.
        for allowed in ["::ffff:140.82.121.4", "::ffff:10.0.0.5"] {
            let addr: std::net::IpAddr = allowed.parse().unwrap();
            assert!(table.dialable_addr(addr).is_ok(), "{allowed}");
        }
        // Genuine v6: the v6 rules still govern (loopback refused,
        // global allowed).
        assert!(table.dialable_addr("::1".parse().unwrap()).is_err());
        assert!(table
            .dialable_addr("2001:db8:1::1".parse().unwrap())
            .is_ok());
    }

    #[test]
    fn the_public_lane_admits_every_target_but_the_metadata_names() {
        // bd myconfig-fvi: the public lane's admission is
        // "every target"; the cloud-metadata NAMES are the one
        // policy refusal at admit time (their IPs sit in classes
        // the dial refuses anyway, but the name is how a trusted
        // resolver would hand the endpoint over). An IP-LITERAL
        // target is admitted here — the deny set classifies the
        // resolved address, and the public lane has no names to
        // match (the table lane's "the name is the policy" rule
        // does not apply).
        let policy = Policy::Public;
        assert!(policy.admit("github.com", 443).is_ok());
        assert!(policy.admit("140.82.121.4", 443).is_ok());
        assert!(policy.admit("anything.example", 8080).is_ok());
        for name in ["metadata.google.internal", "metadata.azure.internal"] {
            let e = policy.admit(name, 80).unwrap_err();
            assert!(e.contains("metadata"), "{name}: {e}");
            // The case-insensitive spelling refuses too.
            let upper = name.to_ascii_uppercase();
            assert!(policy.admit(&upper, 80).is_err(), "{upper}");
        }
    }

    #[test]
    fn the_public_lane_refuses_the_private_and_special_use_ranges() {
        // bd myconfig-fvi: the public lane's deny set — the
        // host-service classes every lane refuses PLUS the private/
        // special-use ranges the microvm precedent drops: RFC1918,
        // CGNAT 100.64/10, 240/4, ULA fc00::/7, and the embedded
        // spellings (mapped v6, NAT64 well-known and local-translation
        // prefixes). The TABLE lane keeps those dialable on purpose
        // (`allow-domains = ["proxy.internal"]` is a documented use),
        // pinned by the inverse assertions.
        let public = Policy::Public;
        for refused in [
            // RFC1918.
            "10.0.0.5",
            "10.255.255.255",
            "172.16.0.1",
            "172.31.255.255",
            "192.168.1.1",
            // CGNAT 100.64/10 (and not the neighboring 100.0.0.1).
            "100.64.0.1",
            "100.127.255.255",
            // 240/4.
            "240.0.0.1",
            "255.0.0.1",
            // The mapped spellings run the same rule set.
            "::ffff:10.0.0.5",
            "::ffff:192.168.1.1",
            "::ffff:100.64.0.1",
            "::ffff:240.0.0.1",
            // ULA fc00::/7 (both halves).
            "fc00::1",
            "fd12:3456:789a::1",
            // NAT64 well-known 64:ff9b::/96 embedding a private v4.
            "64:ff9b::a00:5",
            "64:ff9b::192.168.1.1",
            // Local-translation 64:ff9b:1::/96 embedding one.
            "64:ff9b:1::a00:5",
            // The rest of 64:ff9b:1::/48: unassigned, refused.
            "64:ff9b:1:1::1",
        ] {
            let addr: std::net::IpAddr = refused.parse().unwrap();
            let e = public.dialable_addr(addr).unwrap_err();
            assert!(
                e.contains("public lane") || e.contains("refused address class"),
                "{refused}: {e}"
            );
        }
        for allowed in [
            // A public v4 and v6.
            "140.82.121.4",
            "1.1.1.1",
            "2606:4700::1111",
            // 100.0.0.1 is NOT CGNAT (the /10 starts at 100.64).
            "100.0.0.1",
            // NAT64 embedding a PUBLIC v4 is the translation
            // mechanism working.
            "64:ff9b::8ca5:2004",
            "64:ff9b:1::8ca5:2004",
            // The mapped spellings of public addresses.
            "::ffff:140.82.121.4",
        ] {
            let addr: std::net::IpAddr = allowed.parse().unwrap();
            assert!(public.dialable_addr(addr).is_ok(), "{allowed}");
        }
        // The TABLE lane keeps the private ranges dialable on purpose
        // (its documented use), and refuses only the host-service
        // classes — the inverse pin.
        let table =
            Policy::Table(ConnectTable::validate(&["proxy.internal".to_owned()], &[443]).unwrap());
        assert!(table.dialable_addr("10.0.0.5".parse().unwrap()).is_ok());
        assert!(table
            .dialable_addr("::ffff:10.0.0.5".parse().unwrap())
            .is_ok());
        assert!(table.dialable_addr("127.0.0.1".parse().unwrap()).is_err());
    }

    #[test]
    fn the_public_lane_names_no_fence_ports_and_says_so() {
        // The public lane's rule is the deny set on the resolved
        // address, not a port fence: `dial_ports` is empty (the
        // upstream-only fence stays) and the listen line says what
        // the rule is instead of claiming a widening the kernel does
        // not grant.
        let public = Policy::Public;
        assert!(public.dial_ports().is_empty());
        let summary = public.fence_summary();
        assert!(summary.contains("deny set"), "{summary}");
        assert!(!summary.contains("443"), "{summary}");
        // No lane at all: the phase-B broker.
        assert_eq!(Policy::None.dial_ports(), Vec::<u16>::new());
        assert!(!Policy::None.serves_connect());
        assert!(Policy::Public.serves_connect());
    }

    #[test]
    fn the_fence_ports_carry_the_table_rows() {
        // Ports-only and the conjunction: the fence carries every port
        // row.
        let ports = ConnectTable::validate(&[], &[443, 22]).unwrap();
        assert_eq!(ports.dial_ports(), [22, 443]);
        // Domains-only is REFUSED (review finding 3): the unrestricted
        // port axis it implied cannot be expressed in the Landlock
        // fence, so a table naming hosts must name ports too. The
        // refusal names the missing axis.
        let err = ConnectTable::validate(&["github.com".to_owned()], &[]).unwrap_err();
        assert!(err.contains("--allow-connect-port"), "{err}");
        // Both: the conjunction's ports, and the listen line says
        // exactly those.
        let both = ConnectTable::validate(&["github.com".to_owned()], &[443]).unwrap();
        assert_eq!(both.dial_ports(), [443]);
        assert!(
            both.fence_summary().contains("CONNECT table: 443"),
            "{}",
            both.fence_summary()
        );
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
    // ---- second review finding 2: prefix divergence at the first byte ------

    #[test]
    fn prefix_divergence_is_visible_at_the_first_differing_byte() {
        // Second review finding 2 on 3cfc1bbb3e: the buffering
        // rewrite's prefix check short-circuited `len < 8` to true,
        // so buffers shorter than the method token were ALL held as
        // maybe-CONNECT — a one-byte `G` did not fall back, `GET /` did
        // not fall back — and a non-CONNECT client waited for a
        // divergence its own first byte had already made. The
        // contract (read_preface's doc): the fallback fires as soon
        // as the divergence is visible, which is the first byte that
        // differs.
        for impossible in [
            &b"G"[..],
            b"GE",
            b"GET",
            b"GET ",
            b"GET /",
            // The method is case-SENSITIVE (RFC 7231): the lowercase
            // spelling is a divergence, not a CONNECT.
            b"connect ",
        ] {
            assert!(
                !could_start_connect(impossible),
                "{impossible:?} has diverged from CONNECT already"
            );
        }
        for possible in [
            &b""[..],
            b"C",
            b"CON",
            b"CONNECT",
            b"CONNECT ",
            b"CONNECT example.com:443 HTTP/1.1\r\n",
        ] {
            assert!(
                could_start_connect(possible),
                "{possible:?} could still become CONNECT"
            );
        }
    }

    // ---- second review finding 1: coalesced head + payload ------------------

    /// A socketpair standing in for the broker's client stream — the
    /// raw `socketpair(2)` idiom, the crate's zero-dependency way to
    /// make two connected UnixStreams. Returns (client-side,
    /// peer-side): the lane owns the first, the test speaks as the
    /// proxy client on the second.
    fn socketpair() -> (UnixStream, UnixStream) {
        use std::os::unix::io::FromRawFd;
        extern "C" {
            fn socketpair(domain: i32, ty: i32, proto: i32, sv: *mut i32) -> i32;
        }
        let mut fds = [0i32; 2];
        let rc = unsafe { socketpair(1, 1, 0, fds.as_mut_ptr()) }; // AF_UNIX, SOCK_STREAM
        assert_eq!(rc, 0, "socketpair failed");
        unsafe {
            (
                UnixStream::from_raw_fd(fds[0]),
                UnixStream::from_raw_fd(fds[1]),
            )
        }
    }

    #[test]
    fn the_tunnel_tail_forwards_a_coalesced_payload_not_just_the_copy() {
        // Second review finding 1 on 3cfc1bbb3e: the finding-4 rewrite
        // made `read_request_head` DISCARD the bytes after the head's
        // terminator, so a proxy client that coalesced its head and
        // its initial payload into one write got a 200 and a tunnel
        // whose first payload was silently swallowed — worse than a
        // stall, because it looks successful. This test drives the
        // lane's byte tail with a socketpair client and a loopback TCP
        // fixture as the dialed peer (the class check refuses
        // loopback for real CONNECT targets, so a dialed full tunnel
        // is not provable offline — this tail is the seam a real host
        // tunnel runs unchanged). The `head_so_far` is the first line
        // only, exactly what `read_preface` hands over: the rest of
        // the head AND the coalesced payload must come out of the
        // CLIENT's stream, with the payload forwarded upstream FIRST.
        let (client, mut peer_client) = socketpair();
        let dir = std::env::temp_dir().join(format!("mysbx-tail-{}-coalesced", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("create the run directory");
        let log = std::sync::Arc::new(
            DecisionLog::open(&dir.join("decisions.jsonl")).expect("open the log"),
        );
        let fixture = std::net::TcpListener::bind("127.0.0.1:0").expect("bind the fixture");
        let fixture_addr = fixture
            .local_addr()
            .expect("fixture addr")
            .to_string()
            .parse::<SocketAddr>()
            .expect("fixture socket addr");
        let fixture_thread = std::thread::spawn(move || {
            let (mut stream, _) = fixture.accept().expect("accept");
            let mut first = [0u8; 64];
            let n = stream.read(&mut first).expect("read the first payload");
            let _ = stream.write_all(&first[..n]);
            let _ = stream.flush();
            let mut rest = Vec::new();
            let _ = stream.read_to_end(&mut rest);
            (first[..n].to_vec(), rest)
        });
        // The coalesced write: the rest of the head, its terminator,
        // and the initial tunnel payload in ONE write — the exact
        // shape the discard hit (the preface consumed only the first
        // line, so headers AND payload are still in the stream).
        peer_client
            .write_all(b"Host: example.com\r\n\r\nINITIAL-PAYLOAD-1")
            .expect("write the coalesced head rest and payload");
        let _ = peer_client.flush();
        // The tail runs on its own thread: it must read the head rest
        // off the client and forward the payload BEFORE the follow-up
        // lands, so `first` below can be asserted byte-for-byte.
        let log_for_tail = log.clone();
        let tail = std::thread::spawn(move || {
            tunnel_tail(
                client,
                TcpStream::connect(fixture_addr).expect("dial the fixture"),
                ConnectTarget {
                    host: "example.com".to_string(),
                    port: 443,
                },
                fixture_addr,
                "pid 0 uid 0",
                &log_for_tail,
                &b"CONNECT example.com:443 HTTP/1.1\r\n".to_vec(),
            );
        });
        std::thread::sleep(Duration::from_millis(150));
        peer_client
            .write_all(b"FOLLOW-UP-2")
            .expect("write the follow-up");
        let _ = peer_client.flush();
        // Half-close the client so the tunnel's copy sees EOF and the
        // fixture's read-to-end ends.
        let _ = peer_client.shutdown(std::net::Shutdown::Both);
        tail.join().expect("the tail finished");
        let (first, rest) = fixture_thread.join().expect("join the fixture");
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(
            first, b"INITIAL-PAYLOAD-1",
            "the coalesced initial payload reaches the dialed peer, byte-for-byte, FIRST"
        );
        assert_eq!(rest, b"FOLLOW-UP-2", "the follow-up payload still arrives");
    }

    #[test]
    fn the_preface_holding_a_payload_behind_the_terminator_hands_it_over() {
        // The second shape of finding 1: the terminator — with a
        // payload behind it — was ALREADY inside `head_so_far` when
        // the lane ran, and the early `return` in the first version
        // of `read_request_head` discarded that suffix the same way.
        // In this shape nothing remains to read from the client: the
        // head and its trailing payload arrived together with the
        // request line the preface consumed.
        let (client, mut peer_client) = socketpair();
        let dir = std::env::temp_dir().join(format!("mysbx-tail-{}-behind", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("create the run directory");
        let log = DecisionLog::open(&dir.join("decisions.jsonl")).expect("open the log");
        let fixture = std::net::TcpListener::bind("127.0.0.1:0").expect("bind the fixture");
        let fixture_addr = fixture
            .local_addr()
            .expect("fixture addr")
            .to_string()
            .parse::<SocketAddr>()
            .expect("fixture socket addr");
        let fixture_thread = std::thread::spawn(move || {
            let (mut stream, _) = fixture.accept().expect("accept");
            let mut all = Vec::new();
            let _ = stream.read_to_end(&mut all);
            all
        });
        // The client's stream is EMPTY and closed: in the real flow
        // `read_preface` already consumed the whole coalesced write
        // from it, so the bytes live ONLY in `head_so_far` — passing
        // them in again would test the copy, not the suffix hand-over.
        // The half-close is how the tail sees EOF on the copy.
        let _ = peer_client.shutdown(std::net::Shutdown::Both);
        tunnel_tail(
            client,
            TcpStream::connect(fixture_addr).expect("dial the fixture"),
            ConnectTarget {
                host: "example.com".to_string(),
                port: 443,
            },
            fixture_addr,
            "pid 0 uid 0",
            &log,
            &b"CONNECT example.com:443 HTTP/1.1\r\n\r\nPAYLOAD-BEHIND".to_vec(),
        );
        let all = fixture_thread.join().expect("join the fixture");
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(
            all, b"PAYLOAD-BEHIND",
            "the payload behind the terminator in the preface is forwarded, not dropped"
        );
    }
}
