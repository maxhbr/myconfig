// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The `config.toml` schema (docs/design/config.md).
//!
//! Parsing is strict: unknown keys, wrong types and unknown enum values are
//! errors, never warnings. A sandbox built from a half-understood
//! configuration would be a sandbox with unknown confinement, so the tool
//! fails fast instead (docs/design/config.md D8, D9).

use crate::toml::{self, Table, Value};
use std::collections::BTreeMap;
use std::fmt;
use std::path::Path;

/// Access mode of a mount.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Ro,
    Rw,
}

impl Mode {
    fn parse(s: &str, at: &str) -> Result<Mode, Error> {
        match s {
            "ro" => Ok(Mode::Ro),
            "rw" => Ok(Mode::Rw),
            other => Err(Error::Schema(format!(
                "{at}: invalid mode `{other}`, expected `ro` or `rw`"
            ))),
        }
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Mode::Ro => "ro",
            Mode::Rw => "rw",
        })
    }
}

/// Which terminal multiplexer the **interactive** payload of a sandbox
/// is (docs/design/config.md D17, cli.md D11) — the generalization of
/// the boolean `workmux` key of D16.
///
/// A closed enum on purpose: a layer may say *which of the payloads
/// this build carries* runs, never a command line (D4: configuration
/// that can execute is configuration that can escape). Every non-
/// [`Multiplexer::None`] value needs an entry pinned by the wrapper
/// ([`Multiplexer::entry_var`]); a selection without one is a refused
/// run, never a silent plain shell.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Multiplexer {
    /// A plain interactive shell — the pre-D16 behaviour, and what an
    /// undecided configuration resolves to.
    None,
    /// Plain tmux, one session per repo, on the in-sandbox socket.
    Tmux,
    /// The workmux session of D16 (sidebar + dashboard).
    Workmux,
    /// herdr (<https://herdr.dev>), the agent multiplexer.
    Herdr,
    /// Agent of Empires (`aoe`), a tmux-based agent session manager.
    Aoe,
    /// Orca (<https://onorca.dev>), the agent orchestrator desktop
    /// app / runtime server (../../services.orca.nix). Unlike the
    /// other four this payload is not a terminal multiplexer at all:
    /// its interactive surface is the Orca runtime server
    /// (`orca serve`) started INSIDE the sandbox, whose pairing
    /// endpoint the operator reaches with the Orca desktop/mobile
    /// client — the headless-server form of the upstream guide
    /// (stablyai/orca `docs/reference/headless-linux-server.md`), see
    /// the entry script `../../nix/orca-entry.nix`.
    Orca,
}

impl Multiplexer {
    /// The accepted spellings, in the order the schema error lists
    /// them. Public so the CLI and the tests name the same set.
    pub const NAMES: &'static [&'static str] = &["tmux", "workmux", "herdr", "aoe", "orca", "none"];

    fn parse(s: &str, at: &str) -> Result<Multiplexer, Error> {
        match s {
            "none" => Ok(Multiplexer::None),
            "tmux" => Ok(Multiplexer::Tmux),
            "workmux" => Ok(Multiplexer::Workmux),
            "herdr" => Ok(Multiplexer::Herdr),
            "aoe" => Ok(Multiplexer::Aoe),
            "orca" => Ok(Multiplexer::Orca),
            other => Err(Error::Schema(format!(
                "{at}: invalid multiplexer `{other}`, expected one of {}",
                Multiplexer::NAMES
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))),
        }
    }

    /// The CLI spelling of the same choice (`--multiplexer <name>`,
    /// cli.md D14): the same closed set as the configuration key
    /// (D4/D17: a layer — or a command line — may say *which of the
    /// payloads this build carries* runs, never a command), with the
    /// usage-error wording of the command line instead of the
    /// schema-error wording of a config file.
    pub fn parse_cli(s: &str) -> Result<Multiplexer, String> {
        Self::parse(s, "--multiplexer").map_err(|e| e.to_string())
    }

    /// The value as written in the configuration — the spelling every
    /// message and the `--verbose` report use.
    pub fn name(self) -> &'static str {
        match self {
            Multiplexer::None => "none",
            Multiplexer::Tmux => "tmux",
            Multiplexer::Workmux => "workmux",
            Multiplexer::Herdr => "herdr",
            Multiplexer::Aoe => "aoe",
            Multiplexer::Orca => "orca",
        }
    }

    /// The wrapper variable pinning this multiplexer's entry script
    /// (docs/design/config.md D17), or `None` for
    /// [`Multiplexer::None`], which needs no payload of its own — the
    /// plain shell is [`crate::bwrap::Params::shell`].
    ///
    /// The CLI reads it (`lib.rs`) and the argv builder names it in
    /// the refusal, so the variable a host must set is never spelled
    /// twice.
    pub fn entry_var(self) -> Option<&'static str> {
        match self {
            Multiplexer::None => None,
            Multiplexer::Tmux => Some("MYSBX_MUX_ENTRY_TMUX"),
            Multiplexer::Workmux => Some("MYSBX_MUX_ENTRY_WORKMUX"),
            Multiplexer::Herdr => Some("MYSBX_MUX_ENTRY_HERDR"),
            Multiplexer::Aoe => Some("MYSBX_MUX_ENTRY_AOE"),
            Multiplexer::Orca => Some("MYSBX_MUX_ENTRY_ORCA"),
        }
    }

    /// The wrapper variable pinning this multiplexer's entry script
    /// INSIDE the podman image — the payload of both podman backends,
    /// which mount nothing of the host store the
    /// [`Multiplexer::entry_var`] path lives in.
    pub fn podman_entry_var(self) -> Option<&'static str> {
        match self {
            Multiplexer::None => None,
            Multiplexer::Tmux => Some("MYSBX_PODMAN_MUX_ENTRY_TMUX"),
            Multiplexer::Workmux => Some("MYSBX_PODMAN_MUX_ENTRY_WORKMUX"),
            Multiplexer::Herdr => Some("MYSBX_PODMAN_MUX_ENTRY_HERDR"),
            Multiplexer::Aoe => Some("MYSBX_PODMAN_MUX_ENTRY_AOE"),
            Multiplexer::Orca => Some("MYSBX_PODMAN_MUX_ENTRY_ORCA"),
        }
    }

    /// Whether this choice replaces the interactive shell with an
    /// entry of its own.
    pub fn starts_a_session(self) -> bool {
        self != Multiplexer::None
    }

    /// The directories whose DIRECT-CHILD unix sockets the payload's
    /// multiplexer may connect to and bind on, per the selected entry
    /// script (bd myconfig-7ov: the first live f13 run died because
    /// the grant followed the tmux dir unconditionally). One entry
    /// script's comment per row — nix/*-entry.nix — plus the bwrap
    /// layout facts.
    ///
    /// The FILESYSTEM side needs no extra grant: every directory here
    /// lies under `/mysbx-home`, and nono's single rw home grant
    /// covers it. These are the UNIX-SOCKET grants under the profile's
    /// `af_unix_mediation = "pathname"` — without the explicit grant
    /// the mediation filter denies connect/bind (the f13 fatal: herdr
    /// could not create its server socket, the run exited 1).
    ///
    /// - tmux / aoe: a tmux server on `$TMUX_TMPDIR`
    ///   (bwrap.rs `MUX_SOCKET_DIR` = `/mysbx-home/.mysbx-tmux`), the
    ///   socket FILE `$TMUX_TMPDIR/socket` — a direct child. aoe drives
    ///   the same server through the same variable.
    /// - workmux: the tmux server's socket dir as above, PLUS the
    ///   payload temp dir `/tmp` (bd myconfig-peo): the sidebar
    ///   daemon binds its snapshot socket at
    ///   `std::env::temp_dir()/workmux-sidebar-<hash>.sock` (upstream
    ///   `daemon.rs socket_path`), the payload env carries no TMPDIR,
    ///   so Rust's `temp_dir()` resolves `/tmp` — the sandbox's
    ///   private tmpfs (bd myconfig-7hh). Without the grant the
    ///   daemon's bind is denied, `ensure_daemon_running` times out
    ///   and `workmux sidebar` dies with "Sidebar daemon failed to
    ///   start"; every sidebar pane client connects to the same
    ///   socket. The grant is contained: `/tmp` is tmpfs-private to
    ///   this one sandbox, and pathname mediation only gates sockets
    ///   the payload could already create abstract-namespace anyway.
    /// - orca: the server keeps NO unix socket of its own below the
    ///   home (its pairing endpoint is TCP loopback; the dbus socket
    ///   name in the log is a HOST path that never exists in the
    ///   view), but the entry still validates `TMUX_TMPDIR` and a
    ///   pane running plain tmux lands on it — keep the mux dir.
    ///   `requireSocketDir` also `mkdir`s it, so the subtree grant is
    ///   a no-op either way without mediation noise.
    /// - herdr: NOT a tmux server — its API sockets are
    ///   `$HOME/.config/herdr/herdr.sock` (server) and
    ///   `.../herdr-client.sock` (the client passes it over SSH),
    ///   both direct children of `/mysbx-home/.config/herdr` (the
    ///   entry script writes the session config there every start,
    ///   `mkdir -p`; the f13 log names both paths verbatim).
    pub fn unix_socket_dirs(self) -> &'static [&'static str] {
        match self {
            Multiplexer::None => &[],
            Multiplexer::Herdr => &["/mysbx-home/.config/herdr"],
            // workmux's own temp-dir socket (bd myconfig-peo, see the
            // rows above) rides next to the tmux server's socket dir.
            Multiplexer::Workmux => &["/mysbx-home/.mysbx-tmux", "/tmp"],
            // Every other entry keeps the tmux socket dir; herdr swaps
            // it (it does not use TMUX_TMPDIR for its own API socket).
            _ => &["/mysbx-home/.mysbx-tmux"],
        }
    }
}

impl fmt::Display for Multiplexer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// How a Wayland display reaches the sandbox (docs/design/config.md D18):
/// `Off` (the default) mounts nothing and forwards nothing, `Waypipe`
/// proxies the host compositor through a waypipe channel whose guest end
/// presents a fake compositor socket inside the sandbox and whose host
/// end (`waypipe client`) runs OUTSIDE the sandbox, next to the host
/// compositor.
///
/// A closed enum on purpose, like [`Multiplexer`]: a layer may say WHICH
/// channel this build carries runs, never a command line (D4). The naming
/// follows waypipe's own and is inverted from the sandbox's intuition:
/// the CLIENT is the compositor-side (host) end, the SERVER the
/// application-side (guest) end that presents the fake compositor to the
/// payload.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Display {
    /// No display: the run is headless, the pre-D18 behaviour and what an
    /// undecided configuration resolves to.
    Off,
    /// The display channel of D18: a per-run waypipe channel between
    /// a host `waypipe client` and the in-sandbox `waypipe server` that
    /// wraps the payload. Needs the waypipe pin (`MYSBX_WAYPIPE`, or
    /// `MYSBX_PODMAN_WAYPIPE` under podman-gvisor); a selection without
    /// one is a refused run.
    Waypipe,
}

impl Display {
    /// The accepted spellings, in the order the schema error lists them.
    pub const NAMES: &'static [&'static str] = &["off", "waypipe"];

    fn parse(s: &str, at: &str) -> Result<Display, Error> {
        match s {
            "off" => Ok(Display::Off),
            "waypipe" => Ok(Display::Waypipe),
            other => Err(Error::Schema(format!(
                "{at}: invalid display `{other}`, expected one of {}",
                Display::NAMES
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))),
        }
    }

    /// The value as written in the configuration — the spelling every
    /// message and the `--verbose` report use.
    pub fn name(self) -> &'static str {
        match self {
            Display::Off => "off",
            Display::Waypipe => "waypipe",
        }
    }

    /// Whether this selection opens a display channel at all.
    pub fn is_waypipe(self) -> bool {
        self == Display::Waypipe
    }
}

impl fmt::Display for Display {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// The sandbox's network policy (docs/design/config.md D20): ONE key
/// with three values, ordered `None < ProxyOnly < Shared`. The bool
/// spellings `false`/`true` stay PERMANENT aliases of `None`/`Shared`
/// — a bool config's generated TOML, argv and output stay
/// byte-identical, and this enum never gains a deprecation for them.
///
/// A layer's `network` is tri-state like `backend`: an omitted key
/// decides nothing (`Option<Network>`), and the shared default is
/// applied AFTER the merge, never inside a layer (config.md D7).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Network {
    /// No network at all: an empty netns on bubblewrap, `--network
    /// none` on the podman backends, no net device on the VM
    /// backends. The `network = false` alias.
    None,
    /// Only the host's LiteLLM endpoint (config.md D20): the bwrap-built
    /// tiers enforce it (an empty netns, the forwarder's Unix socket and
    /// the in-sandbox relay, bd myconfig-mo3.2.6, myconfig-vpi9.1); every
    /// other backend refuses it rather than weaken the profile. The
    /// no-model-credential path (D28).
    ProxyOnly,
    /// The public internet without the host's LAN, private or
    /// metadata ranges (bd myconfig-fvi, the microvm precedent's
    /// privateRanges): the bubblewrap backend keeps the empty netns
    /// and the per-run broker, whose CONNECT lane (`--public`)
    /// admits every target gated by the hard-deny set (RFC1918,
    /// CGNAT, 240/4, ULA, the embedded spellings, the
    /// cloud-metadata names). Every other backend refuses the
    /// value rather than serve it as a silent `none` or a shared
    /// network (D21: never accepted-and-ignored). The order of the
    /// values is the narrowing: none < proxy-only < public < shared.
    Public,
    /// The whole network the backend's transport gives (D21's
    /// allowlist narrows what may be reached). The `network = true`
    /// alias, and the built-in default.
    Shared,
}

impl Network {
    /// The accepted STRING spellings, in the order the schema error
    /// lists them. The bool aliases are not strings: `network = false`
    /// is TOML's boolean false, and the error says so.
    pub const NAMES: &'static [&'static str] = &["none", "proxy-only", "public", "shared"];

    /// Parse the canonical value form
    /// (`none`/`proxy-only`/`public`/`shared`).
    /// The bool aliases are handled by the TOML-type dispatch in
    /// `from_table`; this only sees strings.
    pub fn parse(s: &str, at: &str) -> Result<Network, Error> {
        match s {
            "none" => Ok(Network::None),
            "proxy-only" => Ok(Network::ProxyOnly),
            "public" => Ok(Network::Public),
            "shared" => Ok(Network::Shared),
            other => Err(Error::Schema(format!(
                "{at}: invalid network `{other}`, expected one of {} or a boolean (false = none, true = shared)",
                Network::NAMES
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))),
        }
    }

    /// The canonical value spelling — the one the report, the status
    /// line and every message use. A bool config reports `none` /
    /// `shared`: those are the canonical names the aliases map to.
    pub fn name(self) -> &'static str {
        match self {
            Network::None => "none",
            Network::ProxyOnly => "proxy-only",
            Network::Public => "public",
            Network::Shared => "shared",
        }
    }

    /// Whether the network is shared at all — the predicate the rest of
    /// the crate asks. `false` for
    /// `None`, `ProxyOnly` AND `Public`: both profiles have NO direct
    /// route (the broker's socket is the one channel), so every
    /// "is the network up" question answers no.
    pub fn is_shared(self) -> bool {
        self == Network::Shared
    }

    /// Whether the network is denied outright. Distinct from
    /// `!is_shared()`: `ProxyOnly` and `Public` are not denials, so
    /// the questions stay separate (the allowlist contradiction
    /// check must fire for `None` but not for the profiles).
    pub fn is_none(self) -> bool {
        self == Network::None
    }

    /// Whether this is the public-internet-without-LAN profile (bd
    /// myconfig-fvi): the broker's `--public` CONNECT lane. Not a
    /// predicate the transports ask — the registry row
    /// (`backends::Backend::network_public`) decides who may run it.
    pub fn is_public(self) -> bool {
        self == Network::Public
    }

    /// Whether the profile's transport is the per-run egress broker
    /// (the empty netns plus the bound socket directory, D20): true
    /// for `proxy-only` and `public` — the two profiles the bwrap
    /// argv builds identically from the outside, differing only in
    /// the broker's lane flag.
    pub fn is_brokered(self) -> bool {
        matches!(self, Network::ProxyOnly | Network::Public)
    }
}

impl fmt::Display for Network {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// Which nix store a run's sandbox sees (docs/design/guest-store.md):
/// a closed enum on purpose, like [`Display`] — a layer may say WHICH
/// store, never a path. The VM backends (`krun`, `microvm-qemu`) attach
/// their own store disk in `Baked` mode: an erofs image the Nix
/// wrapper bakes at build time from the tools closure, with its
/// registered db, mounted read-only over `/nix` — writes land on the
/// scratch overlay as usual. `Host` is backends.md D3's default: the
/// whole read-only host `/nix/store` through the virtiofs share.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Store {
    /// The whole read-only host store through the share — the
    /// behaviour every backend had before the key existed, and the
    /// built-in default everywhere a disk cannot attach (an
    /// undecided configuration resolves it through
    /// `Merged::resolved_store`).
    Host,
    /// The guest-own store disk (VM backends only): no host store
    /// path enters the sandbox; the wrapper must pin the image
    /// (`MYSBX_VM_STORE_DISK`) or the run is refused. A non-VM
    /// backend cannot express it — also refused, never dropped.
    Baked,
}

impl Store {
    /// The accepted spellings, in the order the schema error lists them.
    pub const NAMES: &'static [&'static str] = &["host", "baked"];

    fn parse(s: &str, at: &str) -> Result<Store, Error> {
        match s {
            "host" => Ok(Store::Host),
            "baked" => Ok(Store::Baked),
            other => Err(Error::Schema(format!(
                "{at}: invalid store `{other}`, expected one of {}",
                Store::NAMES
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))),
        }
    }

    /// The value as written in the configuration — the spelling every
    /// message and the `--verbose` report use.
    pub fn name(self) -> &'static str {
        match self {
            Store::Host => "host",
            Store::Baked => "baked",
        }
    }

    /// Whether the run asks for the guest-own store disk — the
    /// config-side spelling of the mode the launcher-side
    /// `krun::Params::baked()` decides from the pinned disk; the
    /// two doc comments stay in sync.
    pub fn is_baked(self) -> bool {
        self == Store::Baked
    }
}

impl fmt::Display for Store {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// One additional host path exposed inside the sandbox.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mount {
    /// The host path **as written** in the file: absolute, `~/…` or
    /// relative to the directory of the config file that declared it.
    /// Parsing is string-level only; expansion and canonicalization
    /// happen in `crate::merge` (docs/design/config.md D8), which is the
    /// only place that knows `$HOME` and the file each mount came from.
    pub path: String,
    /// Destination inside the sandbox; `None` means "same path".
    /// Always absolute: it names a path in the sandbox's own filesystem
    /// view, so neither `~/` (no host home in there) nor "relative to the
    /// config file" (a host location) would mean anything
    /// (docs/design/config.md D8).
    pub dest: Option<String>,
    /// Defaults to `ro` (D9: nothing from the host filesystem unless
    /// declared).
    pub mode: Mode,
    /// The kind of the RESOLVED source (bd myconfig-2pv): a regular
    /// file, not a directory. Set by the merge's canonicalization
    /// step (D8 — the only place that touches the filesystem, so it
    /// is also the only place that can know), never by the parser
    /// (string-level only); the parser's own synthetic mounts carry
    /// `false`, and so does every literal the callers that bypass
    /// the merge construct (the argv builders run on `Merged`).
    /// Consumers bind files exactly like directories (bwrap does),
    /// but nono's Landlock grants differ: a directory grant targets
    /// a DIRECTORY and a file grant a single file — the grant
    /// emitters pick the flag from this flag (nono.rs section 4).
    pub file: bool,
}

// The repo itself is deliberately not part of the schema: it is the sidecar's
// repo, always mounted rw at its real host path, not expressible in
// configuration (docs/design/config.md D13).

/// A parsed `config.toml`, from either configuration layer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    /// Sandbox technology; `None` means "not decided by this layer"
    /// (docs/design/cli.md D7: never auto-detected).
    pub backend: Option<String>,
    /// The sandbox's network policy (docs/design/config.md D20), one
    /// of [`Network`]. `None` means "not decided by this layer" —
    /// like `backend`: a layer that does not mention `network` must
    /// not count as an explicit `shared` (which would make an omitted
    /// sidecar value re-enable what the user config denied,
    /// docs/design/config.md D7). The shared default is applied AFTER
    /// the merge (see `crate::merge::merge`), never inside a layer.
    pub network: Option<Network>,
    /// Which multiplexer the *interactive* payload is, instead of a
    /// plain shell (docs/design/config.md D17, cli.md D11). `None`
    /// means "not decided by this layer", like `backend`; the
    /// [`Multiplexer::None`] default is applied after the merge, so a
    /// silent layer never counts as an explicit "plain shell".
    ///
    /// It is not an access grant: it selects a payload from mysbx's
    /// own closure (the pinned `MYSBX_MUX_ENTRY_*`) and adds one
    /// in-sandbox environment variable, so — unlike `network` and
    /// `[env]` — either layer may decide it and the sidecar simply
    /// wins when both do (both layers are trusted, D7).
    pub multiplexer: Option<Multiplexer>,
    /// How a Wayland display reaches the sandbox
    /// (docs/design/config.md D18). `None` means "not decided by this
    /// layer", like `backend` and `multiplexer`; the [`Display::Off`]
    /// default is applied after the merge, so a silent layer never
    /// counts as an explicit "headless".
    ///
    /// Like `multiplexer` it is not a host-access grant beyond the
    /// channel it names: the socket directory it binds holds exactly
    /// one waypipe socket (the guards refuse any configuration that
    /// could put anything else at or below `/mysbx-home/wayland-0`),
    /// and both ends of the channel are binaries from mysbx's own
    /// closure, so either layer may decide it and the sidecar wins
    /// when both do.
    pub display: Option<Display>,
    pub mounts: Vec<Mount>,
    /// Environment forwarded into the sandbox.
    pub env: BTreeMap<String, String>,
    /// Nix settings forwarded into the sandbox (the `[nix]` table):
    /// setting name → value, rendered as `nix.conf` lines — on the
    /// bwrap backends as `NIX_CONFIG` (read IN ADDITION to the
    /// wrapper-pinned `/etc/nix/nix.conf`), on the direct-krun guest
    /// as the only nix configuration it has (bd myconfig-j23). The
    /// same layer rules as `[env]`: both layers declare, the sidecar
    /// may introduce keys the user config does not set but must not
    /// override user-set ones (D7).
    pub nix: BTreeMap<String, String>,
    /// Host directories whose git metadata a repo's `.git` FILE may
    /// point at — the approval list for the external git-dir binds
    /// (review-2 item 1). Written as host paths in the same three
    /// forms as `[[mounts]]` paths (D8): absolute, `~/…` (expanded
    /// against the invoking user's home at run time) or relative to
    /// the config file's directory. A repo-writable `.git` file is
    /// untrusted content (D3): the bind happens only when the
    /// resolved target is at or below an entry of this list in the
    /// user config or the sidecar — `mysbx init` snapshots the
    /// discovered directories into a fresh sidecar so the common
    /// worktree/submodule case works out of the box.
    pub git_dirs: Vec<String>,
    /// State directories (docs/design/config.md D15): paths relative
    /// to the sandbox home whose content should persist across runs.
    /// mysbx backs each entry with `<sidecar>/state/<entry>` on the
    /// host, creates it before the backend starts and binds it `rw`
    /// at `/mysbx-home/<entry>` — a sandboxed agent keeps its sessions
    /// and caches per repository, without any host-home path entering
    /// the sandbox. Both layers declare; the lists concatenate like
    /// mounts. Unlike `[[mounts]]` paths these are NEVER resolved
    /// against the host (D8 does not apply): the host path is
    /// synthesized from the sidecar, and the only thing a layer may
    /// say is the shape of the path below the sandbox home.
    pub state_dirs: Vec<String>,
    /// Host environment variables to forward into the sandbox when
    /// actually set at launch (docs/plan.md "Environment"): the
    /// allowlist `collect_host_env` reads. Names only — the VALUES
    /// still come from the host environment at launch time, never from
    /// the config (a credential lives only in the host environment,
    /// never in a store path, so `[env]` cannot forward it and this
    /// list names it instead). The list CONCATENATES onto the built-in
    /// default (`FORWARDED_ENV_VARS`, lib.rs): a layer ADDS names
    /// without restating the technical terminal/locale block. Secrets
    /// are never in that default — naming one here is the ONLY way it
    /// reaches a sandbox. Both layers
    /// declare; the lists concatenate like `state-dirs` (order-stable,
    /// first occurrence wins on duplicates).
    pub forward_env: Vec<String>,
    /// Domains the sandbox may connect to (`allow-domains`,
    /// bd myconfig-mo3.1): a backend-agnostic schema key — every
    /// backend parses it, and enforcement is per backend. A run
    /// REFUSES a finer policy on a backend that cannot enforce it
    /// (the pipeline in lib.rs, not the schema here).
    pub allow_domains: Vec<String>,
    /// TCP ports the sandbox may connect out to (`connect-ports`,
    /// bd myconfig-mo3.1): same backend-agnostic declaration as
    /// [`Config::allow_domains`].
    pub connect_ports: Vec<u16>,
    /// TCP ports the sandbox may listen on (`listen-ports`,
    /// bd myconfig-mo3.1): same backend-agnostic declaration as
    /// [`Config::allow_domains`].
    pub listen_ports: Vec<u16>,
    /// Host services the broker forwards to (`[[host-services]]`,
    /// docs/design/config.md D34, bd myconfig-db56): NAMED host
    /// loopback endpoints, in the same class as `allow-domains` —
    /// declared per layer, merged per the D15 rule, and enforced (or
    /// refused) per backend. The ADDRESS is fixed by the design (host
    /// loopback, always): a row names a service and its HOST port,
    /// never an address, so no spelling of the key can widen a row
    /// into a non-loopback grant.
    pub host_services: Vec<HostServiceRow>,
    /// Memory limit of the sandbox (`memory`, podman `--memory`
    /// grammar: bytes or a `b`/`k`/`m`/`g` suffix, e.g. `"8g"`).
    /// `None` means "not decided by this layer"; the sidecar wins
    /// when both layers decide (D23). Only the podman backends
    /// enforce it; the others warn.
    pub memory: Option<String>,
    /// The per-run nix scratch disk cap (`scratch-size`, podman
    /// `--memory` grammar). Same layering as `memory`; the wrapper's
    /// `MYSBX_KRUN_SCRATCH_SIZE` pin overrides both layers.
    pub scratch_size: Option<String>,
    /// CPU limit (`cpus`, a positive number; podman `--cpus`, or
    /// the whole vCPUs of a krun VM). Same layering as `memory`.
    pub cpus: Option<String>,
    /// Process limit (`pids-limit`, podman `--pids-limit`). Same
    /// layering as `memory`; only `podman-gvisor` enforces it.
    pub pids_limit: Option<u32>,
    /// Whether the guest of a `microvm-qemu` run gets hardware
    /// virtualization (`nested-virt`, docs/design/config.md D27): a
    /// boolean, tri-state in a layer like `network`. `Some(true)` asks
    /// for `/dev/kvm` in the guest (`-cpu host`, `accel=kvm`), so VMs can
    /// be developed and tested inside the sandbox; the default is
    /// `false` because nested VMX widens the host KVM attack surface
    /// and `-cpu host` exposes the host CPU model. A user-config
    /// `false` is a host-wide deny the sidecar cannot lift (the same
    /// narrowing rule as `network`); either layer may otherwise
    /// enable it. The two direct VM backends (`microvm-qemu`,
    /// `microvm-ch`) honor the key — every other backend warns and
    /// drops it, never blocking the run over a missing feature
    /// (krun: libkrun's `krun_set_nested_virt` is macOS-only in the
    /// pinned 1.17.4, libkrunfw 5.3.0 ships CONFIG_KVM unset).
    pub nested_virt: Option<bool>,
    /// Which nix store the sandbox sees (`store`, docs/design/
    /// guest-store.md): `None` means "not decided by this layer", like
    /// `backend` — the [`Store::Host`] default is applied after the
    /// merge, so a silent layer never counts as an explicit `host`.
    /// Either layer may decide it and the sidecar wins when both do
    /// (the key selects a store mysbx itself builds — it grants no
    /// host access beyond what the mode already carries, so it needs
    /// no `network`-style narrowing rule); `baked` warns and falls
    /// back to `host` on backends that cannot attach a store disk.
    pub store: Option<Store>,
    /// The parent configuration this file inherits from (`parent`,
    /// docs/design/config.md D29): a path in the D8 spellings
    /// (absolute, `~/…`, or relative to this file's own directory)
    /// naming another TOML loaded directly BEFORE this file, with
    /// the same overwrite rules between the pair as between the
    /// user config and the sidecar. Chains are supported; a missing,
    /// unreadable, cycling or repo-inside parent is a hard error at
    /// load time (see the chain loader in `crate::merge`). `None` on a
    /// folded chain result — the chain is consumed by the fold.
    pub parent: Option<String>,
    /// The launcher profile this configuration requires (`profile`,
    /// docs/design/config.md D30): a free-form name with the
    /// `[a-z][a-z0-9-]*` grammar (the module layer's
    /// `validProfileName` spelling — profile names are host-defined,
    /// not a crate enum). `None` means "not decided by this layer";
    /// either layer may set it and the sidecar wins when both do
    /// (like `backend`/`multiplexer`: the key grants no host access,
    /// it names which launcher binary must have started the run).
    /// ENFORCED at start: the run is refused (exit 70) unless the
    /// invocation actually is that profile's launcher
    /// (`mysbx-<profile>`, detected via the `MYSBX_PROFILE` env pin
    /// with an argv[0] fallback for the unwrapped crate — see
    /// `crate::profile`). It is a consistency contract, not a
    /// security boundary: the actual tool closure is the wrapper's
    /// pinned `MYSBX_TOOLS_PATH` either way.
    pub profile: Option<String>,
    /// Which agent the INTERACTIVE payload's session starts
    /// (`default-agent`, docs/design/config.md D32): a string KIND,
    /// never a command — a layer may say *which of the agents this
    /// build carries* runs (the wrapper pins the registry as
    /// `MYSBX_AGENT_NAMES`/`MYSBX_AGENTS_FILE`), the argv vector
    /// belongs to the entry scripts that resolve the kind (D4:
    /// configuration that can execute is configuration that can
    /// escape). `None` means "not decided by this layer", like
    /// `multiplexer`: either layer may decide it and the sidecar
    /// wins when both do — the key grants no host access, it selects
    /// a payload from mysbx's own closure and resolves to the
    /// `MYSBX_AGENT_*` environment the entry scripts read.
    pub default_agent: Option<String>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            backend: None,
            network: None,
            nested_virt: None,
            multiplexer: None,
            display: None,
            mounts: Vec::new(),
            env: BTreeMap::new(),
            nix: BTreeMap::new(),
            git_dirs: Vec::new(),
            state_dirs: Vec::new(),
            forward_env: Vec::new(),
            allow_domains: Vec::new(),
            connect_ports: Vec::new(),
            listen_ports: Vec::new(),
            host_services: Vec::new(),
            memory: None,
            scratch_size: None,
            cpus: None,
            pids_limit: None,
            store: None,
            parent: None,
            profile: None,
            default_agent: None,
        }
    }
}

/// Why a configuration could not be loaded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// The file is not valid TOML (of the supported subset).
    Syntax(toml::Error),
    /// The file is valid TOML but not a valid configuration.
    Schema(String),
    /// The file could not be read.
    Io(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Syntax(e) => write!(f, "invalid TOML: {e}"),
            Error::Schema(m) => f.write_str(m),
            Error::Io(m) => f.write_str(m),
        }
    }
}

impl std::error::Error for Error {}

impl Config {
    /// Every spelling [`Config::from_table`] accepts at the top level, i.e.
    /// the whole schema. Public so the `init` template can be pinned
    /// against it — a key added to the parser but not to the template is a
    /// key a fresh operator never sees (see `SIDECAR_TEMPLATE_ROOT` in
    /// `lib.rs`, and the test that compares the two).
    pub const KEYS: &'static [&'static str] = &[
        "parent",
        "backend",
        "profile",
        "default-agent",
        "network",
        "nested-virt",
        "multiplexer",
        "display",
        "mounts",
        "env",
        "nix",
        "git-dirs",
        "state-dirs",
        "forward-env",
        "allow-domains",
        "connect-ports",
        "listen-ports",
        "memory",
        "scratch-size",
        "cpus",
        "pids-limit",
        "store",
    ];

    /// Parse a configuration from TOML text.
    pub fn parse(input: &str) -> Result<Config, Error> {
        let root = toml::parse(input).map_err(Error::Syntax)?;
        Config::from_table(&root)
    }

    /// Read and parse a configuration file.
    pub fn load(path: &Path) -> Result<Config, Error> {
        let text = std::fs::read_to_string(path)
            .map_err(|e| Error::Io(format!("cannot read {}: {e}", path.display())))?;
        Config::parse(&text).map_err(|e| match e {
            Error::Schema(m) => Error::Schema(format!("{}: {m}", path.display())),
            Error::Syntax(inner) => {
                Error::Schema(format!("{}: invalid TOML: {inner}", path.display()))
            }
            other => other,
        })
    }

    fn from_table(root: &Table) -> Result<Config, Error> {
        let mut config = Config::default();
        for (key, value) in root {
            match key.as_str() {
                "backend" => config.backend = Some(string(value, "backend")?.to_owned()),
                // The launcher profile (config.md D30): a free-form
                // name with the same grammar the module layer's
                // `validProfileName` spells — shape only, not a crate
                // enum (profile names are host-defined). `once` is
                // refused here too: it would collide with the base
                // once entry's argv[0] spelling (the fallback detector
                // would misparse `mysbx-once` as profile `once`).
                "profile" => {
                    let name = string(value, "profile")?;
                    if crate::profile::valid_name(name) {
                        config.profile = Some(name.to_owned());
                    } else {
                        return Err(Error::Schema(
                            "top level: `profile` must be a lowercase profile name matching \
                             [a-z][a-z0-9-]* and not the reserved `once` — it names the \
                             mysbx-<profile> launcher this configuration requires"
                                .to_owned(),
                        ));
                    }
                }
                // default-agent (D32): a string KIND with the
                // `[a-z][a-z0-9_-]{0,31}` grammar (the registry keys
                // the module layer validates the same way), never a
                // command — shape only, not a closed enum (agent
                // kinds are host-defined, pinned by the wrapper as
                // `MYSBX_AGENT_NAMES`). Resolution against the pin
                // happens at RUN time (lib.rs), where a kind the
                // build does not carry is a refused run, not a parse
                // error: the file is fine, the agent it names is not
                // installed.
                "default-agent" => {
                    let kind = string(value, "default-agent")?;
                    if valid_agent_kind(kind) {
                        config.default_agent = Some(kind.to_owned());
                    } else {
                        return Err(Error::Schema(
                            "top level: `default-agent` must be a lowercase agent kind matching \
                             [a-z][a-z0-9_-]{0,31} — it names an agent of the pinned registry \
                             (MYSBX_AGENT_NAMES), never a command"
                                .to_owned(),
                        ));
                    }
                }
                "network" => config.network = Some(network(value)?),
                "nested-virt" => {
                    config.nested_virt = Some(boolean(value, "nested-virt")?)
                }
                "multiplexer" => {
                    config.multiplexer = Some(Multiplexer::parse(
                        string(value, "multiplexer")?,
                        "multiplexer",
                    )?)
                }
                // The boolean key D17 replaced. It is an unknown key
                // like any other now, but a generic "unknown key"
                // would leave the operator guessing: a config written
                // for the old schema must say what it became — and it
                // must FAIL rather than be ignored, because ignoring
                // it would silently drop the session the file asked
                // for.
                "workmux" => {
                    return Err(Error::Schema(
                        "top level: the `workmux` key was replaced by `multiplexer` \
                         (docs/design/config.md D17): write `multiplexer = \"workmux\"` \
                         instead of `workmux = true`, and `multiplexer = \"none\"` \
                         instead of `workmux = false`"
                            .to_owned(),
                    ))
                }
                "display" => {
                    config.display = Some(Display::parse(string(value, "display")?, "display")?)
                }
                "mounts" => config.mounts = mounts(value)?,
                "env" => config.env = env(table(value, "env")?)?,
                // nix settings have their own table (bd
                // myconfig-j23): NIX_CONFIG is mysbx-generated
                // infrastructure, not a variable a layer may set —
                // a hand-set `[env] NIX_CONFIG` would fight the
                // generated one for the same slot (the last `--setenv`
                // wins, silently), so it is an unknown-key-style
                // refusal naming the replacement.
                "nix" => config.nix = nix_settings(table(value, "nix")?)?,
                "git-dirs" => config.git_dirs = git_dirs(value)?,
                "state-dirs" => config.state_dirs = state_dirs(value)?,
                "forward-env" => config.forward_env = forward_env(value)?,
                "allow-domains" => config.allow_domains = allow_domains(value)?,
                "connect-ports" => config.connect_ports = ports(value, "connect-ports")?,
                "listen-ports" => config.listen_ports = ports(value, "listen-ports")?,
                "host-services" => config.host_services = host_services(value)?,
                "memory" => config.memory = Some(memory(value)?),
                "scratch-size" => {
                    config.scratch_size = Some(scratch_size(value)?)
                }
                "cpus" => config.cpus = Some(cpus(value)?),
                "pids-limit" => config.pids_limit = Some(pids_limit(value)?),
                "store" => {
                    config.store = Some(Store::parse(string(value, "store")?, "store")?)
                }
                // parent (D29): a path in the D8 spellings, naming
                // another TOML loaded directly before this file. The
                // chain loader (merge.rs) resolves and validates it;
                // here it is only the PATH SHAPE that is checked —
                // non-empty, `~` only as a `~/` prefix — the same check
                // every `[[mounts]]` path gets.
                "parent" => {
                    config.parent = Some(host_path(string(value, "parent")?, "parent")?)
                }
                // The config key D22 replaced: the sandbox's own SSH
                // keypair is ALWAYS generated now, so the key decides
                // nothing anymore. A config written for the old schema
                // must FAIL rather than be silently dropped, and say
                // what became of it — the same treatment as the
                // replaced `workmux` key above.
                "ssh-key" => {
                    return Err(Error::Schema(
                        "top level: the `ssh-key` key is obsolete — the sandbox's own SSH keypair \n                         is now ALWAYS generated into <repo>.mysbx/state/.ssh and bound at the \n                         sandbox's ~/.ssh (docs/design/config.md D22); remove the line"
                            .to_owned(),
                    ))
                }
                other => return Err(unknown("top level", other)),
            }
        }
        // `network = "proxy-only"` names the ONE reachable endpoint
        // (docs/design/config.md D20/D21, egress-connect.md E7, bd
        // myconfig-ew9s; review finding 5): D21's allowlist keys next
        // to `network = "proxy-only"` are NO LONGER a same-file
        // schema error — the compatibility of the two is a property of
        // the MERGED configuration (the pairing is legal on
        // `bubblewrap`, refused everywhere else), and a file-local
        // check ran too early: a user layer with `backend =
        // "bubblewrap"` plus a sidecar with the profile and keys was
        // rejected BEFORE the merge, while the identical effective
        // config with a redundant backend key in the sidecar passed.
        // The refusal lives in the post-merge runtime gate (lib.rs
        // step 4b), where merged.backend and merged.network are both
        // known — still ahead of the session clone and the
        // `--dry-run` return, so a dry run audits it.
        Ok(config)
    }
}

fn mounts(value: &Value) -> Result<Vec<Mount>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "mounts: expected an array of tables ([[mounts]]), found {}",
            value.type_name()
        ))
    })?;
    let mut out = Vec::with_capacity(items.len());
    for (n, item) in items.iter().enumerate() {
        let at = format!("[[mounts]] #{}", n + 1);
        let t = item.as_table().ok_or_else(|| {
            Error::Schema(format!(
                "{at}: expected a table, found {}",
                item.type_name()
            ))
        })?;
        let mut path = None;
        let mut dest = None;
        let mut mode = Mode::Ro;
        for (key, value) in t {
            match key.as_str() {
                "path" => {
                    path = Some(host_path(
                        string(value, &format!("{at}: path"))?,
                        &format!("{at}: path"),
                    )?)
                }
                "dest" => {
                    dest = Some(absolute(
                        string(value, &format!("{at}: dest"))?,
                        &format!("{at}: dest"),
                    )?)
                }
                "mode" => mode = Mode::parse(string(value, &format!("{at}: mode"))?, &at)?,
                other => return Err(unknown(&at, other)),
            }
        }
        let path =
            path.ok_or_else(|| Error::Schema(format!("{at}: missing required key `path`")))?;
        out.push(Mount {
            file: false,
            path,
            dest,
            mode,
        });
    }
    Ok(out)
}

/// Parse the `git-dirs` approval list: an array of host-path strings,
/// each in one of the D8 forms (absolute, `~/…`, relative). The same
/// `host_path` shape check as `[[mounts]]` paths applies; canonicalization
/// happens in the merge, eagerly (D8), so a dangling approval is a hard
/// error rather than a silent no-op.
fn git_dirs(value: &Value) -> Result<Vec<String>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "git-dirs: expected an array of strings, found {}",
            value.type_name()
        ))
    })?;
    items
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let at = format!("git-dirs #{}", i + 1);
            host_path(string(v, &at)?, &at)
        })
        .collect()
}

/// Parse the `forward-env` list: an array of environment-variable
/// names. The values are never part of the config — they are read
/// from the host environment at launch time, so no credential can
/// enter a config file (or a store path) through this key.
fn forward_env(value: &Value) -> Result<Vec<String>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "forward-env: expected an array of strings, found {}",
            value.type_name()
        ))
    })?;
    items
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let at = format!("forward-env #{}", i + 1);
            let name = string(v, &at)?;
            if name.is_empty() {
                return Err(Error::Schema(format!(
                    "{at}: expected a non-empty environment variable name"
                )));
            }
            Ok(name.to_owned())
        })
        .collect()
}

/// Parse the `allow-domains` list (bd myconfig-mo3.1): an array of
/// domain names the sandbox may connect to. Backend-agnostic by
/// design — enforcement happens per backend, so a backend that
/// cannot enforce the policy refuses the run downstream instead of
/// silently ignoring entries here.
fn allow_domains(value: &Value) -> Result<Vec<String>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "allow-domains: expected an array of strings, found {}",
            value.type_name()
        ))
    })?;
    items
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let at = format!("allow-domains #{}", i + 1);
            let domain = string(v, &at)?;
            if domain.is_empty() {
                return Err(Error::Schema(format!("{at}: expected a non-empty domain")));
            }
            Ok(domain.to_owned())
        })
        .collect()
}

/// One `[[host-services]]` row (docs/design/config.md D34, bd
/// myconfig-db56): a NAMED host service the broker forwards to.
/// The address is deliberately not spellable — the design fixes host
/// loopback — so the row carries a name and a HOST port only, and
/// the broker argv pins `name=127.0.0.1:<port>`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HostServiceRow {
    /// The row's selector: the plain host-name grammar of
    /// `allow-domains` (lower-cased by the merge), never `localhost`
    /// (inside the sandbox that name IS the relay's own listener).
    pub name: String,
    /// The host port the service listens on (1-65535).
    pub port: u16,
}

/// Parse the `[[host-services]]` table array (D34): strict schema
/// (D11) — `name` and `port` are both required, unknown keys and
/// wrong types are errors naming the file, the key and the row. The
/// per-row name shape check runs here so a bad name never reaches
/// the merge, and a name repeated WITHIN one file is a schema error;
/// a name repeated ACROSS layers is the merge's to judge (D15's
/// first-win, or a refusal when the ports differ).
fn host_services(value: &Value) -> Result<Vec<HostServiceRow>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "host-services: expected an array of tables ([[host-services]]), found {}",
            value.type_name()
        ))
    })?;
    let mut out = Vec::with_capacity(items.len());
    for (n, item) in items.iter().enumerate() {
        let at = format!("[[host-services]] #{}", n + 1);
        let t = item.as_table().ok_or_else(|| {
            Error::Schema(format!(
                "{at}: expected a table, found {}",
                item.type_name()
            ))
        })?;
        let mut name = None;
        let mut port = None;
        for (key, value) in t {
            match key.as_str() {
                "name" => name = Some(string(value, &format!("{at}: name"))?),
                "port" => {
                    let p = value.as_integer().ok_or_else(|| {
                        Error::Schema(format!(
                            "{at}: port: expected an integer, found {}",
                            value.type_name()
                        ))
                    })?;
                    if !(1..=65535).contains(&p) {
                        return Err(Error::Schema(format!(
                            "{at}: port: expected a port between 1 and 65535, found {p}"
                        )));
                    }
                    port = Some(p as u16);
                }
                other => return Err(unknown(&at, other)),
            }
        }
        let name =
            name.ok_or_else(|| Error::Schema(format!("{at}: missing required key `name`")))?;
        let port =
            port.ok_or_else(|| Error::Schema(format!("{at}: missing required key `port`")))?;
        host_service_name(&name, &at)?;
        let name = name.to_ascii_lowercase();
        if out.iter().any(|r: &HostServiceRow| r.name == name) {
            return Err(Error::Schema(format!(
                "{at}: name `{name}` is already named by an earlier row — a row's \
                 name selects exactly one forward (docs/design/config.md D34)"
            )));
        }
        out.push(HostServiceRow { name, port });
    }
    Ok(out)
}

/// The `[[host-services]]` name shape (D34): the `allow-domains`
/// grammar — a plain host name, no scheme, path, port or whitespace —
/// never an IP literal (the name is the selector, an address would
/// make the row an address grant) and never `localhost` (inside the
/// sandbox that name is the in-sandbox relay's own listener, bound
/// over /etc/hosts).
fn host_service_name(name: &str, at: &str) -> Result<(), Error> {
    let bad = |why: &str| {
        Error::Schema(format!(
            "{at}: name {why} — a row's name is the selector the payload \
             CONNECTs to (docs/design/config.md D34)"
        ))
    };
    if name.is_empty() {
        return Err(bad("must not be empty"));
    }
    if name.eq_ignore_ascii_case("localhost") {
        return Err(bad(
            "must not be `localhost` — inside the sandbox that name is the \
             relay's own listener, not a host service",
        ));
    }
    if name.contains("://")
        || name.contains('/')
        || name.contains(':')
        || name.contains('=')
        || name.chars().any(|c| c.is_whitespace() || c.is_control())
        || !name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '.')
    {
        return Err(bad(
            "must be a plain host name — no scheme, path, port or gateway syntax",
        ));
    }
    if name.parse::<std::net::IpAddr>().is_ok() {
        return Err(bad("must not be an IP literal — the name is the policy"));
    }
    Ok(())
}

/// Parse `memory`: a string in podman's `--memory` grammar — a whole
/// number of bytes, optionally with a `b`, `k`, `m` or `g` suffix
/// (any case), e.g. `"8g"`, `"512m"`. `pub(crate)`: the session
/// picker's typed row parses its draft through the SAME grammar
/// at Enter (docs/design/tui.md T10).
pub(crate) fn memory(value: &Value) -> Result<String, Error> {
    let raw = string(value, "memory")?;
    let lower = raw.to_ascii_lowercase();
    let digits = lower.strip_suffix(['b', 'k', 'm', 'g']).unwrap_or(&lower);
    match digits.parse::<u64>() {
        Ok(n) if n > 0 => Ok(raw.to_owned()),
        _ => Err(Error::Schema(format!(
            "memory: expected a size like \"8g\" or \"512m\" \
             (a positive number of bytes with an optional b/k/m/g suffix), found {raw:?}"
        ))),
    }
}

/// Parse `scratch-size`: the `memory` grammar, with the scratch's
/// own 64 MiB floor (an ext4 that small cannot serve as the nix
/// scratch — the same floor `MYSBX_KRUN_SCRATCH_SIZE` enforces).
fn scratch_size(value: &Value) -> Result<String, Error> {
    let raw = memory(value)?;
    let lower = raw.to_ascii_lowercase();
    let (digits, multiplier) = if let Some(d) = lower.strip_suffix('g') {
        (d, 1024 * 1024 * 1024)
    } else if let Some(d) = lower.strip_suffix('m') {
        (d, 1024 * 1024)
    } else if let Some(d) = lower.strip_suffix('k') {
        (d, 1024)
    } else if let Some(d) = lower.strip_suffix('b') {
        (d, 1)
    } else {
        (lower.as_str(), 1)
    };
    match digits
        .parse::<u64>()
        .ok()
        .and_then(|n| n.checked_mul(multiplier))
    {
        Some(bytes) if bytes >= 64 * 1024 * 1024 => Ok(raw.to_owned()),
        _ => Err(Error::Schema(format!(
            "scratch-size: expected a size like \"32g\" (at least 64m), found {raw:?}"
        ))),
    }
}

/// Parse `cpus`: a positive integer or float, kept as the string
/// podman's `--cpus` takes. `pub(crate)`: the session picker's typed
/// row parses its draft through the SAME grammar at Enter
/// (docs/design/tui.md T10).
pub(crate) fn cpus(value: &Value) -> Result<String, Error> {
    let n = match value {
        Value::Integer(i) => *i as f64,
        Value::Float(f) => *f,
        other => {
            return Err(Error::Schema(format!(
                "cpus: expected a number, found {}",
                other.type_name()
            )))
        }
    };
    if !(n.is_finite() && n > 0.0) {
        return Err(Error::Schema(format!(
            "cpus: expected a positive number, found {n}"
        )));
    }
    Ok(match value {
        Value::Integer(i) => i.to_string(),
        _ => n.to_string(),
    })
}

/// Parse `pids-limit`: a positive integer. `pub(crate)`: the session
/// picker's typed row parses its draft through the SAME grammar at
/// Enter (docs/design/tui.md T10).
pub(crate) fn pids_limit(value: &Value) -> Result<u32, Error> {
    let n = value.as_integer().ok_or_else(|| {
        Error::Schema(format!(
            "pids-limit: expected an integer, found {}",
            value.type_name()
        ))
    })?;
    u32::try_from(n).ok().filter(|n| *n > 0).ok_or_else(|| {
        Error::Schema(format!(
            "pids-limit: expected a positive integer, found {n}"
        ))
    })
}

/// Parse a port list, `connect-ports` or `listen-ports`
/// (bd myconfig-mo3.1): an array of TCP port numbers, each 1-65535.
fn ports(value: &Value, key: &str) -> Result<Vec<u16>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "{key}: expected an array of integers, found {}",
            value.type_name()
        ))
    })?;
    items
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let at = format!("{key} #{}", i + 1);
            let n = v.as_integer().ok_or_else(|| {
                Error::Schema(format!(
                    "{at}: expected an integer, found {}",
                    v.type_name()
                ))
            })?;
            if !(1..=65535).contains(&n) {
                return Err(Error::Schema(format!(
                    "{at}: expected a port between 1 and 65535, found {n}"
                )));
            }
            Ok(n as u16)
        })
        .collect()
}

/// Parse the `state-dirs` list (docs/design/config.md D15): an array
/// of sandbox-home-relative paths. See [`state_dir_path`] for the
/// per-entry shape check.
fn state_dirs(value: &Value) -> Result<Vec<String>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "state-dirs: expected an array of strings, found {}",
            value.type_name()
        ))
    })?;
    items
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let at = format!("state-dirs #{}", i + 1);
            state_dir_path(string(v, &at)?, &at)
        })
        .collect()
}

/// A `state-dirs` entry (docs/design/config.md D15): a path *relative
/// to the sandbox home* naming where the persistent directory is bound
/// (`/mysbx-home/<entry>`), with the host backing store synthesized by
/// mysbx at `<sidecar>/state/<entry>`. It is neither a host path (so
/// the D8 forms do not apply) nor a free-form in-sandbox `dest`: the
/// host side is decided by the sidecar (D2/D10), the sandbox side by
/// D14. The only thing a layer may say is the shape of the path
/// below the home — and it must be unambiguous, because the runtime
/// joins the entry into both trees: a leading `/`, a `~/` prefix and
/// `.`/`..`/empty components are rejected here, so no entry can climb
/// out of the sandbox home or of the sidecar's state tree, whatever
/// joins it.
fn state_dir_path(path: &str, at: &str) -> Result<String, Error> {
    let bad = |why: &str| {
        Error::Schema(format!(
            "{at}: must be a relative path below the sandbox home ({why}): `{path}`"
        ))
    };
    if path.is_empty() {
        return Err(bad("must not be empty"));
    }
    if path.starts_with('/') {
        return Err(bad(
            "no leading `/` — the destination is always /mysbx-home/<entry>",
        ));
    }
    if path.starts_with('~') {
        return Err(bad(
            "no `~/` prefix — the sandbox home is not the host home",
        ));
    }
    for component in path.split('/') {
        if component.is_empty() {
            return Err(bad("no empty `//` components"));
        }
        if component == "." || component == ".." {
            return Err(bad("no `.` or `..` components"));
        }
    }
    Ok(path.to_owned())
}

/// The `[nix]` table: nix setting names → values. A key must be a
/// usable setting name — `nix.conf` keys are `identifier = value`
/// lines, so the same refusal reasons as the `[env]` key check apply,
/// plus nix's own grammar (no `#`/`;`: a value smuggling a comment
/// or a second directive would corrupt the generated file).
fn nix_settings(t: &Table) -> Result<BTreeMap<String, String>, Error> {
    let mut out = BTreeMap::new();
    for (key, value) in t {
        if key.is_empty()
            || key.starts_with('-')
            || key.contains('=')
            || key.contains('#')
            || key.contains(';')
        {
            return Err(Error::Schema(format!(
                "nix.{key:?} is not a usable nix setting name"
            )));
        }
        let value = string(value, &format!("nix.{key}"))?;
        // nix parses NIX_CONFIG as a config FILE: a newline in a
        // value would smuggle a second directive (or a `#` comment
        // would eat the rest of a line). One setting, one line —
        // strict parsing (D11), at the schema edge.
        if value.contains('\n') || value.contains('\r') || value.contains('#') {
            return Err(Error::Schema(format!(
                "nix.{key}: the value must be a single directive-free line — \
                 found a newline or a `#` in {value:?}"
            )));
        }
        out.insert(key.clone(), value.to_owned());
    }
    Ok(out)
}

fn env(t: &Table) -> Result<BTreeMap<String, String>, Error> {
    let mut out = BTreeMap::new();
    for (key, value) in t {
        // Keys become `--setenv` arguments in the bwrap argv
        // (src/bwrap.rs); one that looks like a flag, or contains `=`
        // (ambiguous parsing), or is empty, would smuggle data past the
        // option parser. TOML bare keys allow all of these, so reject
        // them here, at the schema edge, where every other impossible
        // value is rejected too.
        if key.is_empty() || key.starts_with('-') || key.contains('=') {
            return Err(Error::Schema(format!(
                "env key {key:?} is not a usable variable name"
            )));
        }
        // NIX_CONFIG is mysbx-generated from the `[nix]` table (bd
        // myconfig-j23): a hand-set entry would fight the generated
        // one for the same slot (the later --setenv wins, silently).
        // The `workmux` treatment: FAIL, and say the replacement.
        if key == "NIX_CONFIG" {
            return Err(Error::Schema(
                "env.NIX_CONFIG is mysbx-generated from the [nix] table \
                 (bd myconfig-j23): write `[nix]` settings instead of \
                 setting the variable"
                    .to_owned(),
            ));
        }
        out.insert(
            key.clone(),
            string(value, &format!("env.{key}"))?.to_owned(),
        );
    }
    Ok(out)
}

// ---- small typed accessors -----------------------------------------------

fn string<'v>(value: &'v Value, at: &str) -> Result<&'v str, Error> {
    value.as_str().ok_or_else(|| {
        Error::Schema(format!(
            "{at}: expected a string, found {}",
            value.type_name()
        ))
    })
}

fn boolean(value: &Value, at: &str) -> Result<bool, Error> {
    value.as_bool().ok_or_else(|| {
        Error::Schema(format!(
            "{at}: expected a boolean, found {}",
            value.type_name()
        ))
    })
}

/// The `network` key (docs/design/config.md D20): a string
/// (`none`/`proxy-only`/`shared`) or a bool, whose `false`/`true` are
/// PERMANENT aliases of `none`/`shared` — never deprecated, so a bool
/// config keeps its byte-identical output. Any other type or string is
/// a D11 schema error naming the key.
fn network(value: &Value) -> Result<Network, Error> {
    if let Some(b) = value.as_bool() {
        return Ok(if b { Network::Shared } else { Network::None });
    }
    if let Some(s) = value.as_str() {
        return Network::parse(s, "network");
    }
    Err(Error::Schema(format!(
        "network: expected \"none\", \"proxy-only\", \"shared\" or a boolean, found {}",
        value.type_name()
    )))
}

fn table<'v>(value: &'v Value, at: &str) -> Result<&'v Table, Error> {
    value.as_table().ok_or_else(|| {
        Error::Schema(format!(
            "{at}: expected a table, found {}",
            value.type_name()
        ))
    })
}

/// A host path of a `[[mounts]]` entry (docs/design/config.md D8). It may
/// be absolute, `~/…` (the invoking user's home) or relative to the
/// directory of the config file that declared it. Parsing stays
/// string-level: the parser knows neither `$HOME` nor which file it is
/// reading, so expansion and canonicalization happen in `crate::merge`,
/// eagerly, before the merge.
///
/// Rejected here are only the spellings that can never be resolved: the
/// empty string, and a `~` that is not the `~/` prefix — `~` alone and
/// `~user/…` are not supported, because "another user's home" is a
/// lookup this tool deliberately does not do.
fn host_path(path: &str, at: &str) -> Result<String, Error> {
    if path.is_empty() {
        return Err(Error::Schema(format!("{at}: must not be empty")));
    }
    if path.starts_with('~') && !path.starts_with("~/") {
        return Err(Error::Schema(format!(
            "{at}: only the `~/` prefix is supported, not `~` alone or `~user`: `{path}`"
        )));
    }
    Ok(path.to_owned())
}

/// A `dest` is absolute (docs/design/config.md D8). Unlike a mount's
/// host path it is an *in-sandbox* path: there is no host home to expand
/// `~/` against and no config file to be relative to inside the sandbox's
/// filesystem view, and it is never canonicalized against the host (see
/// `crate::merge::canonicalize_layer`). Anything but an absolute path is
/// therefore a configuration mistake and is rejected here.
fn absolute(path: &str, at: &str) -> Result<String, Error> {
    if Path::new(path).is_absolute() {
        Ok(path.to_owned())
    } else {
        Err(Error::Schema(format!(
            "{at}: must be absolute (it is a path inside the sandbox: no `~/`, no relative paths): `{path}`"
        )))
    }
}

fn unknown(at: &str, key: &str) -> Error {
    Error::Schema(format!("{at}: unknown key `{key}`"))
}

/// Whether `kind` is a valid `default-agent` spelling (config.md
/// D32): `[a-z][a-z0-9_-]{0,31}` — lowercase, alphanumerics, `_` and
/// `-`, at most 32 characters. Public so the CLI validation and the
/// module layer's Nix assertion can name the same grammar; shared
/// with the schema edge above.
pub fn valid_agent_kind(kind: &str) -> bool {
    let mut chars = kind.chars();
    match chars.next() {
        Some(c) if c.is_ascii_lowercase() => {}
        _ => return false,
    }
    let tail: Vec<char> = chars.collect();
    tail.len() <= 31
        && tail
            .iter()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || *c == '_' || *c == '-')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn profile_parses_as_a_grammar_validated_name() {
        // config.md D30 (bd myconfig-011r): a free-form profile name
        // with the `[a-z][a-z0-9-]*` grammar, NOT a crate enum — the
        // crate validates shape only. `once` is reserved (it would
        // collide with the base once entry's argv[0] spelling).
        let c = Config::parse("profile = \"assistant\"\n").unwrap();
        assert_eq!(c.profile.as_deref(), Some("assistant"));
        let c = Config::parse("profile = \"agent-42\"\n").unwrap();
        assert_eq!(c.profile.as_deref(), Some("agent-42"));
        assert_eq!(Config::parse("").unwrap().profile, None);
        // Every refusal is a Schema error naming the key.
        for text in [
            "profile = \"\"\n",
            "profile = \"Assistant\"\n",
            "profile = \"-x\"\n",
            "profile = \"a b\"\n",
            "profile = \"once\"\n",
            "profile = 42\n",
            "profile = true\n",
        ] {
            let e = Config::parse(text).unwrap_err();
            let msg = e.to_string();
            assert!(matches!(e, Error::Schema(_)), "{text}: {msg}");
            assert!(msg.contains("profile"), "{text}: {msg}");
        }
    }

    #[test]
    fn default_agent_parses_as_a_grammar_validated_kind() {
        // config.md D32: a free-form agent KIND with the
        // `[a-z][a-z0-9_-]{0,31}` grammar, NOT a crate enum — the
        // crate validates shape only; the run-time resolution checks
        // the kind against the wrapper's `MYSBX_AGENT_NAMES` pin.
        let c = Config::parse("default-agent = \"pi\"\n").unwrap();
        assert_eq!(c.default_agent.as_deref(), Some("pi"));
        let c = Config::parse("default-agent = \"opencode\"\n").unwrap();
        assert_eq!(c.default_agent.as_deref(), Some("opencode"));
        let c = Config::parse("default-agent = \"a_kind-42\"\n").unwrap();
        assert_eq!(c.default_agent.as_deref(), Some("a_kind-42"));
        assert_eq!(Config::parse("").unwrap().default_agent, None);
        // Every refusal is a Schema error naming the key.
        for text in [
            "default-agent = \"\"\n",
            "default-agent = \"Pi\"\n",
            "default-agent = \"-x\"\n",
            "default-agent = \"a b\"\n",
            "default-agent = \"pi --continue\"\n",
            "default-agent = \"012345678901234567890123456789012\"\n",
            "default-agent = 42\n",
            "default-agent = true\n",
        ] {
            let e = Config::parse(text).unwrap_err();
            let msg = e.to_string();
            assert!(matches!(e, Error::Schema(_)), "{text}: {msg}");
            assert!(msg.contains("default-agent"), "{text}: {msg}");
        }
    }

    #[test]
    fn empty_config_is_all_defaults() {
        let c = Config::parse("").unwrap();
        assert_eq!(c, Config::default());
        // `network` is tri-state in a layer: None means "not decided" —
        // the shared-by-default `true` is applied after the merge.
        assert_eq!(c.network, None);
        assert_eq!(c.nested_virt, None, "nested-virt is tri-state too");
        assert!(c.backend.is_none());
    }

    #[test]
    fn nested_virt_parses_as_a_boolean_and_rejects_other_shapes() {
        // D27: a plain boolean key, tri-state in a layer like `network`.
        let c = Config::parse("nested-virt = true\n").unwrap();
        assert_eq!(c.nested_virt, Some(true));
        let c = Config::parse("nested-virt = false\n").unwrap();
        assert_eq!(c.nested_virt, Some(false));
        // A wrong type names the key, like every other schema error.
        let e = Config::parse("nested-virt = \"yes\"\n").unwrap_err();
        let msg = e.to_string();
        assert!(matches!(e, Error::Schema(_)), "{msg}");
        assert!(msg.contains("nested-virt"), "{msg}");
    }

    #[test]
    fn mount_mode_defaults_to_ro() {
        let c = Config::parse("[[mounts]]\npath = \"/etc/hosts\"\n").unwrap();
        assert_eq!(c.mounts[0].mode, Mode::Ro);
        assert_eq!(c.mounts[0].dest, None);
    }

    #[test]
    fn multiplexer_is_a_tri_state_enum() {
        // docs/design/config.md D17: like `backend`, an omitted key
        // decides nothing — the `none` default is applied after the
        // merge, so a layer never counts as an explicit "plain shell".
        assert_eq!(Config::parse("").unwrap().multiplexer, None);
        for (text, want) in [
            ("tmux", Multiplexer::Tmux),
            ("workmux", Multiplexer::Workmux),
            ("herdr", Multiplexer::Herdr),
            ("aoe", Multiplexer::Aoe),
            ("orca", Multiplexer::Orca),
            ("none", Multiplexer::None),
        ] {
            let c = Config::parse(&format!("multiplexer = \"{text}\"\n")).unwrap();
            assert_eq!(c.multiplexer, Some(want), "{text}");
            // The spelling round-trips: it is what the report and
            // every message print.
            assert_eq!(want.name(), text);
        }
        // Every accepted spelling is in NAMES, and nothing else is.
        assert_eq!(Multiplexer::NAMES.len(), 6);
    }

    #[test]
    fn multiplexer_rejects_unknown_values_and_wrong_types() {
        // Strict enum (D9/D11/D17): an unknown value names the key and
        // lists what is accepted, so a typo is fixable from the error.
        for bad in ["screen", "zellij", "", "TMUX", "workmux "] {
            let e = Config::parse(&format!("multiplexer = \"{bad}\"\n")).unwrap_err();
            let msg = e.to_string();
            assert!(matches!(e, Error::Schema(_)), "{bad:?}: {msg}");
            assert!(msg.contains("multiplexer"), "{bad:?}: {msg}");
            for name in Multiplexer::NAMES {
                assert!(msg.contains(name), "{bad:?}: {msg} does not list {name}");
            }
        }
        // Wrong types name the key, like every other schema error.
        let e = Config::parse("multiplexer = true\n").unwrap_err();
        assert!(e.to_string().contains("multiplexer"), "{e}");
        assert!(matches!(e, Error::Schema(_)), "{e}");
    }

    #[test]
    fn the_old_workmux_key_names_its_replacement() {
        // D17 superseded the boolean key. A config written for the old
        // schema must fail LOUDLY — ignoring it would drop the session
        // it asked for — and the message must say what to write.
        for text in ["workmux = true\n", "workmux = false\n"] {
            let e = Config::parse(text).unwrap_err();
            let msg = e.to_string();
            assert!(matches!(e, Error::Schema(_)), "{msg}");
            assert!(msg.contains("multiplexer"), "{msg}");
            assert!(msg.contains("D17"), "{msg}");
        }
    }

    #[test]
    fn entry_variables_are_distinct_and_only_none_has_none() {
        // The pins the wrapper sets (D17). One per session-starting
        // value, all distinct: a shared variable would make one
        // multiplexer's pin start another's payload.
        let mut seen: Vec<&str> = Vec::new();
        for name in Multiplexer::NAMES {
            let m = Multiplexer::parse(name, "test").unwrap();
            match m.entry_var() {
                None => assert_eq!(m, Multiplexer::None),
                Some(var) => {
                    assert!(m.starts_a_session());
                    assert!(var.starts_with("MYSBX_MUX_ENTRY_"), "{var}");
                    assert!(!seen.contains(&var), "duplicate pin {var}");
                    seen.push(var);
                }
            }
        }
        assert_eq!(seen.len(), 5);
    }

    #[test]
    fn podman_entry_variables_mirror_the_bwrap_pins() {
        for name in Multiplexer::NAMES {
            let m = Multiplexer::parse(name, "test").unwrap();
            assert_eq!(
                m.podman_entry_var().map(str::to_string),
                m.entry_var()
                    .map(|v| v.replace("MYSBX_MUX_ENTRY_", "MYSBX_PODMAN_MUX_ENTRY_")),
            );
        }
    }

    #[test]
    fn display_is_a_tri_state_enum() {
        // docs/design/config.md D18: like `backend`, an omitted key
        // decides nothing — the `off` default is applied after the merge.
        assert_eq!(Config::parse("").unwrap().display, None);
        for (text, want) in [("off", Display::Off), ("waypipe", Display::Waypipe)] {
            let c = Config::parse(&format!("display = \"{text}\"\n")).unwrap();
            assert_eq!(c.display, Some(want), "{text}");
            assert_eq!(want.name(), text);
            assert_eq!(want.is_waypipe(), text == "waypipe");
        }
        assert_eq!(Display::NAMES.len(), 2);
    }

    #[test]
    fn display_rejects_unknown_values_and_wrong_types() {
        // Strict enum (D9/D11/D18): an unknown value names the key and
        // lists what is accepted, so a typo is fixable from the error.
        for bad in ["x11", "wayland", "", "Waypipe", "waypipe "] {
            let e = Config::parse(&format!("display = \"{bad}\"\n")).unwrap_err();
            let msg = e.to_string();
            assert!(matches!(e, Error::Schema(_)), "{bad:?}: {msg}");
            assert!(msg.contains("display"), "{bad:?}: {msg}");
            for name in Display::NAMES {
                assert!(msg.contains(name), "{bad:?}: {msg} does not list {name}");
            }
        }
        let e = Config::parse("display = true\n").unwrap_err();
        assert!(e.to_string().contains("display"), "{e}");
        assert!(matches!(e, Error::Schema(_)));
    }

    #[test]
    fn unknown_keys_are_rejected() {
        assert!(matches!(Config::parse("nope = 1\n"), Err(Error::Schema(_))));
    }

    #[test]
    fn repo_table_is_rejected() {
        // The repo is implicit (docs/design/config.md D13); a `[repo]` table is
        // an unknown top-level key and therefore a schema error (D11).
        assert!(matches!(Config::parse("[repo]\n"), Err(Error::Schema(_))));
        assert!(matches!(
            Config::parse("[repo]\npath = \"/home/user/src/project\"\n"),
            Err(Error::Schema(_))
        ));
    }

    #[test]
    fn relative_and_home_paths_are_accepted_verbatim() {
        // The parser stores the path as written; `crate::merge` expands
        // `~/` and resolves relative paths against the config file's
        // directory (docs/design/config.md D8).
        let c = Config::parse("[[mounts]]\npath = \"rel/sub\"\n").unwrap();
        assert_eq!(c.mounts[0].path, "rel/sub");
        let c = Config::parse("[[mounts]]\npath = \"~/.config/git\"\n").unwrap();
        assert_eq!(c.mounts[0].path, "~/.config/git");
        let c = Config::parse("[[mounts]]\npath = \"../outside\"\n").unwrap();
        assert_eq!(c.mounts[0].path, "../outside");
    }

    #[test]
    fn unsupported_tilde_spellings_are_rejected() {
        for p in ["~", "~other", "~other/data"] {
            let e = Config::parse(&format!("[[mounts]]\npath = \"{p}\"\n")).unwrap_err();
            assert!(e.to_string().contains("only the `~/` prefix"), "{p}: {e}");
        }
    }

    // ---- state-dirs (docs/design/config.md D15) -----------------------

    #[test]
    fn state_dirs_parse_as_home_relative_paths() {
        let c =
            Config::parse("state-dirs = [\".local/share/opencode\", \".local/state/opencode\"]\n")
                .unwrap();
        assert_eq!(
            c.state_dirs,
            vec![".local/share/opencode", ".local/state/opencode"]
        );

        // A single-element array and an empty array both parse; the
        // empty list is also the default.
        let c = Config::parse("state-dirs = [\".cache/build\"]\n").unwrap();
        assert_eq!(c.state_dirs, vec![".cache/build"]);
        let c = Config::parse("state-dirs = []\n").unwrap();
        assert!(c.state_dirs.is_empty());
        assert!(Config::default().state_dirs.is_empty());
    }

    #[test]
    fn state_dirs_reject_non_string_entries() {
        assert!(Config::parse("state-dirs = [1]\n").is_err());
        assert!(Config::parse("state-dirs = \".cache\"\n").is_err());
        // A wrong type names the key, like every other schema error.
        let e = Config::parse("state-dirs = true\n").unwrap_err();
        assert!(e.to_string().contains("state-dirs"), "{e}");
    }

    #[test]
    fn state_dirs_reject_climbing_and_ambiguous_spellings() {
        // The entry is joined into TWO trees (the sidecar's state dir on
        // the host, the sandbox home below the tmpfs), so every spelling
        // that could make either join escape is rejected at the schema
        // edge (D15): absolute (a host-path confusion), `~/`, empty,
        // `//` runs, `.` and `..`.
        for p in [
            "/abs", "/", "~/x", "", "a//b", "a/./b", "../x", "a/../b", "x/..",
        ] {
            let e = Config::parse(&format!("state-dirs = [\"{p}\"]\n")).unwrap_err();
            let msg = e.to_string();
            assert!(msg.contains("state-dirs"), "{p:?}: {msg}");
            assert!(msg.contains("sandbox home"), "{p:?}: {msg}");
        }
    }

    // ---- ssh-key (docs/design/config.md D22) ----------------------------

    #[test]
    fn ssh_key_is_refused_as_obsolete() {
        // The keypair is unconditional now: a config that still sets
        // the old `ssh-key` key must FAIL, naming what to remove.
        let e = Config::parse("ssh-key = true\n").unwrap_err();
        let msg = e.to_string();
        assert!(msg.contains("obsolete"), "{msg}");
        assert!(Config::parse("ssh-key = false\n").is_err());
    }

    // ---- allow-domains / connect-ports / listen-ports (bd
    // myconfig-mo3.1) -------------------------------------------

    #[test]
    fn allowlist_keys_parse() {
        let c = Config::parse(
            "allow-domains = [\"example.org\", \"api.example.org\"]\n\
             connect-ports = [443, 22]\n\
             listen-ports = [8080]\n",
        )
        .unwrap();
        assert_eq!(c.allow_domains, vec!["example.org", "api.example.org"]);
        assert_eq!(c.connect_ports, vec![443, 22]);
        assert_eq!(c.listen_ports, vec![8080]);

        // Empty arrays and the omitted keys are the empty default.
        let c =
            Config::parse("allow-domains = []\nconnect-ports = []\nlisten-ports = []\n").unwrap();
        assert!(c.allow_domains.is_empty());
        assert!(c.connect_ports.is_empty());
        assert!(c.listen_ports.is_empty());
        assert!(Config::default().allow_domains.is_empty());
        assert!(Config::default().connect_ports.is_empty());
        assert!(Config::default().listen_ports.is_empty());
    }

    #[test]
    fn allow_domains_rejects_wrong_shapes() {
        // Non-string and empty entries name the key and the entry.
        let e = Config::parse("allow-domains = [1]\n").unwrap_err();
        let msg = e.to_string();
        assert!(matches!(e, Error::Schema(_)), "{msg}");
        assert!(msg.contains("allow-domains"), "{msg}");

        let e = Config::parse("allow-domains = [\"\"]\n").unwrap_err();
        let msg = e.to_string();
        assert!(matches!(e, Error::Schema(_)), "{msg}");
        assert!(msg.contains("allow-domains #1"), "{msg}");
        assert!(msg.contains("non-empty domain"), "{msg}");

        // A wrong top-level type names the key, like every other one.
        let e = Config::parse("allow-domains = \"example.org\"\n").unwrap_err();
        assert!(e.to_string().contains("allow-domains"), "{e}");
        assert!(matches!(e, Error::Schema(_)), "{e}");
    }

    #[test]
    fn port_lists_reject_out_of_range_and_wrong_shapes() {
        for (key, bad) in [
            ("connect-ports", "0"),
            ("connect-ports", "65536"),
            ("connect-ports", "\"443\""),
            ("listen-ports", "0"),
            ("listen-ports", "65536"),
            ("listen-ports", "true"),
        ] {
            let e = Config::parse(&format!("{key} = [{bad}]\n")).unwrap_err();
            let msg = e.to_string();
            assert!(matches!(e, Error::Schema(_)), "{key} = {bad}: {msg}");
            assert!(msg.contains(key), "{key} = {bad}: {msg}");
        }
        // The error names the entry.
        let e = Config::parse("connect-ports = [443, 0]\n").unwrap_err();
        assert!(e.to_string().contains("connect-ports #2"), "{e}");
        let e = Config::parse("listen-ports = [\"x\"]\n").unwrap_err();
        let msg = e.to_string();
        assert!(msg.contains("listen-ports #1"), "{msg}");
        assert!(msg.contains("expected an integer"), "{msg}");

        // A wrong top-level type names the key.
        let e = Config::parse("connect-ports = 443\n").unwrap_err();
        assert!(e.to_string().contains("connect-ports"), "{e}");
        assert!(matches!(e, Error::Schema(_)), "{e}");

        // The boundaries are inclusive.
        let c = Config::parse("connect-ports = [1, 65535]\nlisten-ports = [1]\n").unwrap();
        assert_eq!(c.connect_ports, vec![1, 65535]);
        assert_eq!(c.listen_ports, vec![1]);
    }

    #[test]
    fn resource_limits_parse() {
        let c = Config::parse("memory = \"8g\"\ncpus = 4\npids-limit = 512\n").unwrap();
        assert_eq!(c.memory.as_deref(), Some("8g"));
        assert_eq!(c.cpus.as_deref(), Some("4"));
        assert_eq!(c.pids_limit, Some(512));
        let c = Config::parse("memory = \"1073741824\"\ncpus = 1.5\n").unwrap();
        assert_eq!(c.memory.as_deref(), Some("1073741824"));
        assert_eq!(c.cpus.as_deref(), Some("1.5"));
        assert_eq!(Config::parse("").unwrap().memory, None);
    }

    #[test]
    fn resource_limits_reject_wrong_shapes() {
        for (body, key) in [
            ("memory = 8", "memory"),
            ("memory = \"8x\"", "memory"),
            ("memory = \"0\"", "memory"),
            ("memory = \"g\"", "memory"),
            ("cpus = \"4\"", "cpus"),
            ("cpus = 0", "cpus"),
            ("cpus = -1.5", "cpus"),
            ("pids-limit = 0", "pids-limit"),
            ("pids-limit = 1.5", "pids-limit"),
            ("pids-limit = \"512\"", "pids-limit"),
        ] {
            let e = Config::parse(&format!("{body}\n")).unwrap_err();
            assert!(e.to_string().starts_with(key), "{body}: {e}");
        }
    }

    #[test]
    fn empty_path_is_rejected() {
        let e = Config::parse("[[mounts]]\npath = \"\"\n").unwrap_err();
        assert!(e.to_string().contains("must not be empty"), "{e}");
    }

    #[test]
    fn scratch_size_takes_the_memory_grammar_with_a_64m_floor() {
        // D23: the same b/k/m/g grammar as `memory`, plus the
        // scratch's own floor — the pin's (an ext4 that small
        // cannot serve as the nix scratch).
        for raw in ["32g", "100G", "512m", "64m", "67108864"] {
            let c = Config::parse(&format!("scratch-size = \"{raw}\"\n")).unwrap();
            assert_eq!(c.scratch_size.as_deref(), Some(raw), "{raw}");
        }
        for raw in ["63m", "1024", "garbage"] {
            let e = Config::parse(&format!("scratch-size = \"{raw}\"\n")).unwrap_err();
            assert!(
                e.to_string().contains(raw),
                "the refusal names the offending value: {raw}: {e}"
            );
        }
        // The floor's OWN refusal (a value the memory grammar
        // accepts but the scratch cannot serve):
        let e = Config::parse("scratch-size = \"32k\"\n").unwrap_err();
        assert!(e.to_string().contains("scratch-size"), "{e}");
    }

    #[test]
    fn non_absolute_dest_is_rejected() {
        // `dest` is an in-sandbox path and stays absolute-only.
        for d in ["rel", "~/inside"] {
            let e =
                Config::parse(&format!("[[mounts]]\npath = \"/a\"\ndest = \"{d}\"\n")).unwrap_err();
            assert!(e.to_string().contains("must be absolute"), "{d}: {e}");
        }
    }

    #[test]
    fn parent_parses_as_a_path_and_rejects_other_shapes() {
        // D29: a single string in the D8 spellings — absolute, `~/…`,
        // or relative to the declaring file's own directory (the
        // loader resolves that last one, not the parser). Only the
        // path SHAPE is checked here, exactly like a `[[mounts]]`
        // path; the loader adds existence, cycle and containment.
        for raw in [
            "/etc/mysbx/base.toml",
            "~/config/mysbx/base.toml",
            "base.toml",
        ] {
            let c = Config::parse(&format!("parent = \"{raw}\"\n")).unwrap();
            assert_eq!(c.parent.as_deref(), Some(raw), "input {raw:?}");
        }
        // A wrong type names the key, like every other schema error.
        let e = Config::parse("parent = 3\n").unwrap_err();
        let msg = e.to_string();
        assert!(matches!(e, Error::Schema(_)), "{msg}");
        assert!(msg.contains("parent"), "{msg}");
        // The same `host_path` shape rules as `[[mounts]]` paths: an
        // empty string decides nothing and must fail loudly (a typo'd
        // `parent = ""` would otherwise silently load no parent), and
        // `~`-but-not-`~/` is the one rejected `~` spelling.
        let e = Config::parse("parent = \"\"\n").unwrap_err();
        assert!(e.to_string().contains("must not be empty"), "{e}");
        let e = Config::parse("parent = \"~user/base.toml\"\n").unwrap_err();
        assert!(
            e.to_string().contains("only the `~/` prefix is supported"),
            "{e}"
        );
    }

    #[test]
    fn keys_lists_exactly_the_accepted_top_level_spellings() {
        // The list exists for the `init` template check; it must not drift
        // from `from_table`. A key the parser accepts but `KEYS` omits
        // would silently fall out of the template again.
        for key in Config::KEYS {
            assert!(
                !matches!(Config::parse(&format!("{key} = 1\n")), Err(Error::Schema(m)) if m.contains("unknown key")),
                "`{key}` is in KEYS but the parser calls it unknown"
            );
        }
        // And the reverse, over a spelling that is NOT a key.
        let not_a_key = "egress";
        assert!(!Config::KEYS.contains(&not_a_key));
        assert!(matches!(
            Config::parse(&format!("{not_a_key} = \"x\"\n")),
            Err(Error::Schema(m)) if m.contains("unknown key")
        ));
    }
}
