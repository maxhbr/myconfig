// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The podman + gVisor (runsc) backend argv builder.
//!
//! Precedent: the gvisor tier (modules/myconfig.ai.dev/sandboxes/
//! myconfig.ai.gvisor-agent-sandbox/, tier 3.5) runs rootless podman
//! with the runsc runtime, supports `--mount HOST:DEST[:ro|rw]`,
//! network isolation (`--network=none` or pasta spec), and resource
//! limits (`--memory/--cpus/--pids-limit`).
//!
//! This backend maps the mysbx merged config (mounts, network policy,
//! env, state-dirs, multiplexer payload) onto a podman invocation with
//! the runsc runtime. It is the SECOND backend and doubles as the proof
//! that the backend seam is general.
//!
//! The argv builder is pure — it canonicalizes nothing and checks no
//! existence (the merge already did that, docs/design/config.md D8) —
//! so tests can assert the exact argument vector without spawning podman.
//!
//! Sections, in fixed order (order is semantic for overlapping binds):
//!
//! 0. `run` and its global args (`--runtime=runsc`, `--runtime-flag`
//!    per flag, `--cgroup-manager` when one is set — from env vars,
//!    see below) — WITHOUT the program name: the returned argv is
//!    arguments-only, like bwrap's (`--clearenv` first there), because
//!    lib.rs prepends the backend binary itself via
//!    `Command::new(MYSBX_PODMAN)`.
//! 1. container identity: `--replace`, `--name`, `--hostname`, `--userns=keep-id`
//! 2. base isolation: `--read-only`, `--read-only-tmpfs=true`,
//!    `--cap-drop=ALL`, `--security-opt=no-new-privileges`
//! 3. working directory: `--workdir` at the repo path (the container's
//!    view of the workspace)
//! 4. container home: a fresh empty `type=tmpfs` mount at
//!    [`CONTAINER_HOME`] — this backend's equivalent of bwrap's
//!    tmpfs `SANDBOX_HOME` (config.md D14): everything the XDG base
//!    dirs name lives on a writable surface without a single host
//!    file entering through it — plus tmpfs mounts at the XDG
//!    `.local/share`/`.local/state` parents
//!    ([`CONTAINER_DATA_HOME`]/[`CONTAINER_STATE_HOME`], bd
//!    myconfig-e50: runsc would create those bind-mountpoint parents
//!    root-owned, and the container user could not create siblings
//!    under them) and at the parent of every `state-dirs` entry that
//!    lives below the home (the same root-owned-mountpoint failure,
//!    generalized from the XDG pair: tmpfs the DEEPEST state
//!    ancestor, never the read-only config surface — bd
//!    myconfig-ixz)
//! 5. workspace bind: the repo (or clone) mounted at its own path
//!    (config.md D13, workspace.md D3), plus git metadata dirs when
//!    approved, plus the worktrees sibling when it exists, plus
//!    state-dirs binds (config.md D15)
//! 6. configured mounts, in declaration order (config.md D7/D8),
//!    `--mount type=bind,src=HOST,dst=DEST,ro|rw`
//! 7. environment: host-forwarded first, then `cfg.env`, then the
//!    backend pins of `MYSBX_GVISOR_ENV`, then infrastructure
//!    variables (`HOME`, the XDG base dirs derived from it, `PATH`,
//!    CA-bundle vars, `TMUX_TMPDIR` for a multiplexer session)
//! 8. resource limits: `--pids-limit`, `--memory`, `--cpus` (when
//!    cgroups are not ignored)
//! 9. network: `--network` spec (shared by default, or `none` / pasta
//!    spec when `network = false` or configured)
//! 10. image reference (from config or default)
//! 11. payload — WITHOUT a `--` separator before it: the first token
//!     after the image IS the command (podman, unlike docker, does
//!     not strip a stray `--`); the multiplexer entry (for interactive
//!     sessions) or the image's shell/command
//!
//! Deliberately different from bubblewrap:
//!
//! - Uses a container image instead of host PATH: the image's own
//!   userland provides the shell, the tool `PATH` and the TLS trust
//!   anchors — NOTHING is bind-mounted from the host /nix/store
//!   (the same policy as the gvisor tier: "host binaries must not be
//!   bind-mounted"), and that is why the payload/PATH pins of this
//!   backend are image paths, not the host store paths of bwrap's
//!   `Params` (bd myconfig-wao)
//! - Container runtime lifecycle (podman manages the container)
//! - Container-user identity (via `--userns=keep-id`)
//! - User-space kernel (gVisor's runsc)
//!
//! The backend seam must NOT bake in "argv builder that execs directly
//! as your uid with a bind-mounted CWD" — this backend proves the seam
//! is general.

use crate::bwrap::{Payload, PolicyPath, Waypipe, Workspace};
use crate::config::{Mode, Mount, Multiplexer};
use crate::merge::Merged;
use crate::repo::Repo;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::fmt;
use std::path::{Path, PathBuf};

/// Host environment values the operator chose to forward, keyed by
/// variable name. The builder is pure, so it cannot read `std::env`
/// itself; item 5 collects the forwarded host variables
/// (`FORWARDED_ENV_VARS`, lib.rs — the terminal/locale block plus the
/// model-credential block of bd myconfig-20j, each only when set) into
/// this map.
pub type HostEnv = BTreeMap<String, String>;

/// The sandbox's own home directory (docs/design/config.md D14, the
/// `$HOME` row of the base table in docs/plan.md).
///
/// A path **inside** the container, not a host path: section 4 mounts
/// a fresh empty tmpfs there (the image carries no `/mysbx-home`),
/// and we set `HOME` to it via `--env`. The path deliberately lives
/// outside `/home`, so
/// nothing inside the sandbox can be confused with a host home path:
/// the invariant "no IN-SANDBOX path under `/home/`" (config.md D14)
/// stays literally checkable on the argv's destinations — mount
/// *sources* are host paths and may of course live in the host home.
pub const CONTAINER_HOME: &str = "/mysbx-home";

/// The directory holding the private socket of a multiplexer payload
/// (docs/design/config.md D16, generalized by D17), exported as
/// `TMUX_TMPDIR` and used by the entry scripts as
/// `tmux -S $TMUX_TMPDIR/socket`.
///
/// It is a path **inside** the container home and nothing else:
/// no host path is bound at or below it, and no configuration may
/// make one land there (see [`check_mux_socket`]). The socket is
/// therefore reachable only from this one sandbox — never from
/// another mysbx sandbox of the same repository, never from a tmux
/// server on the host, and it dies with the container. That is the
/// whole isolation claim of the multiplexer integration, and it rests
/// on the path being infrastructure: `TMUX_TMPDIR` is emitted after
/// `[env]`, like `HOME` and `PATH`, so no layer can repoint it at
/// `/tmp/tmux-1000` or at a bound host directory.
///
/// The value is set for EVERY session-starting choice, including the
/// one that is not a tmux server (`herdr`, which keeps its own socket
/// and state under the tmpfs `HOME`): one code path, and a
/// pane that runs plain `tmux` inside such a session lands on this
/// private socket instead of the host default `/tmp/tmux-<uid>`.
pub const MUX_SOCKET_DIR: &str = "/mysbx-home/.mysbx-tmux";

/// The `WAYLAND_DISPLAY` name the guest waypipe server presents its
/// fake compositor socket under (docs/design/config.md D18), the same
/// spelling the bwrap backend uses — a bare name, so the multi-mode
/// server creates it in `$XDG_RUNTIME_DIR`.
pub const WAYPIPE_DISPLAY: &str = "wayland-0";

/// The in-container path of the waypipe display socket:
/// `$XDG_RUNTIME_DIR/<WAYPIPE_DISPLAY>` with `XDG_RUNTIME_DIR` pinned
/// at [`CONTAINER_HOME`] (D18). No bind ever lands at or below it; the
/// socket is created by the multi-mode in-container waypipe server.
pub const WAYPIPE_DISPLAY_PATH: &str = "/mysbx-home/wayland-0";

/// The `XDG_DATA_HOME` path inside the container home (bd
/// myconfig-e50): section 4 mounts a tmpfs at it next to the home
/// tmpfs itself, because runsc CREATES the missing mountpoint dirs
/// of a bind root-owned — a `.local/share` that exists only to host
/// a `state-dirs` bind is then not creatable-in for the container
/// user (`--userns=keep-id`), and the payload shell dies on its
/// first `mkdir` below it (fish: `EACCES` on `$XDG_DATA_HOME/fish`).
/// The same root-owned-mountpoint failure generalizes to the parent
/// of EVERY `state-dirs` entry below the home
/// ([`state_parent_tmpfses`], bd myconfig-ixz).
pub const CONTAINER_DATA_HOME: &str = "/mysbx-home/.local/share";

/// The `XDG_STATE_HOME` path inside the container home — same
/// treatment as [`CONTAINER_DATA_HOME`] (bd myconfig-e50).
pub const CONTAINER_STATE_HOME: &str = "/mysbx-home/.local/state";

/// The `state-dirs` tmpfs rule (bd myconfig-ixz), generalized from
/// the XDG-parent pair of bd myconfig-e50: state dirs are by design
/// "the writable part of the home", and their PARENT in the
/// container must be as writable as the state dir itself, because
/// real tools keep runtime files as SIBLINGS of the state dir — the
/// pi integration persists `.pi/agent/sessions` while pi writes
/// `auth.json` & co. DIRECTLY into `.pi/agent`, and runsc creates
/// the missing bind-mountpoint ancestors root-owned 0755
/// (`--userns=keep-id`: no write bit for the container user), so pi
/// died with EACCES on its very first runtime file
/// (`models.json error: ... open '/mysbx-home/.pi/agent/auth.json'`).
///
/// The rule tmpfses exactly ONE dest per entry: the entry's PARENT
/// (`$HOME/.pi/agent` for `.pi/agent/sessions`). The shallower
/// ancestors (runsc's own root-owned mountpoint chain, e.g. `.pi`)
/// need no tmpfs: the payload never creates files there itself, it
/// only needs x/search through them — 0755 has it — and its +w
/// lands on the tmpfs'd parent itself. `.config` stays excluded:
/// the ro host-config seed mount is the deliberately read-only
/// surface — a parent gets a tmpfs only when a STATE entry makes it
/// the vehicle of a writable bind, and only parents, never a
/// pure-config ro surface. The XDG
/// `.local/share`/`.local/state` parents are covered by their own
/// unconditional tmpfs mounts ([`CONTAINER_DATA_HOME`],
/// [`CONTAINER_STATE_HOME`], bd myconfig-e50), so they are not
/// re-emitted here. Infrastructure, not binds: the tmpfs dests do not
/// pass [`check_hidden_mounts`]/[`check_symlinkable_dests`] — the
/// `state-dirs` binds of section 5a are emitted after them, and
/// both engines sort mounts parents-before-children (podman's
/// stable same-depth sort keeps the later bind the winner), so
/// persistence semantics are unchanged (the e50 commit message
/// asserted that equal-depth stable sort for the exact-dest case).
fn state_parent_tmpfses(entries: &[String]) -> Vec<String> {
    let covered = [CONTAINER_DATA_HOME, CONTAINER_STATE_HOME];
    let mut dests: Vec<String> = Vec::new();
    for entry in entries {
        let parent = match Path::new(entry).parent() {
            Some(p) => format!("{}/{}", CONTAINER_HOME, p.to_string_lossy()),
            None => continue,
        };
        if parent == CONTAINER_HOME
            || covered.contains(&parent.as_str())
            || dests.iter().any(|d| *d == parent)
        {
            continue;
        }
        dests.push(parent);
    }
    dests
}

/// Common parameters of every invocation that do not come from a
/// configuration layer. Unlike bwrap's `Params` — whose shell and
/// dev-tool `PATH` are HOST store paths the wrapper pins from its own
/// closure — the payload-relevant ones here are **paths inside the
/// container image**: this backend mounts nothing from the host
/// `/nix/store`, so a host store path would die with `no such file or
/// directory` the moment the payload starts (bd myconfig-wao). The
/// defaults mirror the gVisor agent image's own OCI config
/// (agent-image.nix: `Cmd = "/bin/bash"`, `Env =
/// "PATH=/bin:/usr/bin"`) — the same userland the agent-gvisor
/// sessions run against.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Params<'a> {
    /// Path of the shell used for [`Payload::Shell`] — a path INSIDE
    /// the container (default `/bin/bash`, the agent image's `Cmd`).
    pub shell: &'a str,
    /// The container `PATH` (default `/bin:/usr/bin`, the agent
    /// image's `Env`): the image's own `/bin` toolchain — git, tig,
    /// ripgrep, fd, jq, python3, coreutils, … — never a host store
    /// closure.
    pub tools_path: &'a str,
    /// The **trusted policy files** this run was configured from — the
    /// user config and the sidecar config, exactly as `load_layers`
    /// read them, each with the host paths that must stay unwritable
    /// for it to STAY trusted (see [`PolicyPath`]). Empty when a layer
    /// was absent (an absent file grants nothing and needs no
    /// protection).
    ///
    /// The payload must never be able to write these: the user config
    /// is the host-wide grant layer and the sidecar is the one file a
    /// repository's sandbox runs are steered by, and a writable policy
    /// file makes the NEXT run a widened one — a `git-dirs` approval
    /// can be added by the attacker, and the `.git` pointer rewritten
    /// to match (review-3 item 3). `rw` mount sources that contain one
    /// are therefore refused; the check also covers the repo bind
    /// itself, which is `rw` by definition.
    pub policy_paths: &'a [PolicyPath],
    /// The **multiplexer entry** the interactive payload is replaced
    /// by when the merged configuration selects one
    /// (docs/design/config.md D17, cli.md D11) — the entry of THAT
    /// multiplexer, resolved by the caller from
    /// [`Multiplexer::entry_var`], or `None` when this build pinned
    /// none for it.
    ///
    /// Like [`Params::shell`] this is a path INSIDE the container,
    /// pinned by the Nix wrapper (`MYSBX_MUX_ENTRY_*`). No image ships
    /// one today, so a selected multiplexer is a refused run
    /// ([`Error::MultiplexerUnavailable`]) — the same refusal
    /// semantics as a bwrap host that carries no multiplexer: never a
    /// silent plain shell, because the operator asked for a session
    /// and getting a bare shell instead would be discovered only after
    /// the work was done in the wrong place.
    pub mux_entry: Option<&'a str>,
    /// The **waypipe channel** of a run whose merged `display` is
    /// `waypipe` (docs/design/config.md D18): the host socket directory
    /// (bound rw at itself, so the in-container waypipe server finds
    /// the socket where the host client created it) and the guest
    /// `waypipe server` binary — a path INSIDE the container image
    /// (`MYSBX_GVISOR_WAYPIPE`, pinned by the wrapper when the image
    /// carries waypipe). `None` when nothing is pinned: a selected
    /// display is a refused run ([`Error::DisplayUnavailable`]) — the
    /// container mounts nothing from the host `/nix/store`, so no
    /// fallback could exist.
    pub waypipe: Option<Waypipe<'a>>,
    /// Which tree this run works in (workspace.md D1): the live repo
    /// (the default, unchanged) or a named session's clone (D3/D4 —
    /// the argv differences are exactly the workspace's). A
    /// CLI-layer fact, like every other field of `Params`: it does
    /// not come from a configuration layer, and no TOML key can name
    /// it (workspace.md D1).
    pub workspace: Workspace<'a>,
    /// Whether the run attaches the terminal to the container
    /// (`--interactive`): mysbx always execs podman with its own
    /// stdio inherited, so the payload's stdin/stdout/stderr must be
    /// forwarded — without `--interactive` podman closes the
    /// container's stdin and an interactive shell payload reads
    /// instant EOF and exits 0 before any container shows up in
    /// `podman ps` (the f13 silent-exit, bd myconfig-jho). The gvisor
    /// tier's attached runs pass it too (podman.rs `build_run_args`).
    pub interactive: bool,
    /// Whether stdin is a terminal (`--tty`): allocates a pty, so an
    /// interactive payload gets a real terminal instead of a pipe.
    /// Kept separate from [`Params::interactive`] because a one-shot
    /// `run -- CMD` that reads piped stdin must not have a tty
    /// forced on it.
    pub tty: bool,
    /// The container image reference to use (e.g. `localhost/agent-gvisor:latest`).
    /// This is a backend-specific parameter, not part of the generic
    /// merged config — it comes from the backend configuration or a
    /// default. For podman-gvisor, this is the gVisor agent image.
    pub image: &'a str,
    /// Podman runtime flags (e.g. `ignore-cgroups`). These come from
    /// environment variables or backend configuration.
    pub runtime_flags: &'a [String],
    /// The podman `--cgroup-manager` value, or `None` to omit the
    /// flag entirely. The gvisor tier's rootless defaults
    /// (rust/src/state.rs) are the precedent: rootless runs pass
    /// `cgroupfs`, root runs omit the flag (the system cgroup manager
    /// — typically systemd — owns the hierarchy).
    pub cgroup_manager: Option<&'a str>,
    /// Whether cgroups are ignored (runtime flag `ignore-cgroups`).
    /// When true, resource limits are not enforced.
    pub ignore_cgroups: bool,
    /// Network specification: `None` means shared (podman default),
    /// `Some("none")` means no network, or a pasta spec for custom
    /// networking.
    pub network_spec: Option<&'a str>,
    /// Backend-specific environment pins (`MYSBX_GVISOR_ENV`), each a
    /// `KEY=VALUE` string. They are emitted AFTER the config layers'
    /// `[env]` (so a pin wins over a configured value) and BEFORE the
    /// infrastructure variables (so no pin can repoint `HOME`, the XDG
    /// base dirs or `PATH`). What needs them is environment that is
    /// only correct under THIS backend, e.g. the container-side URL of
    /// the host's LiteLLM forwarder.
    pub extra_env: &'a [String],
    /// Resource limits from configuration.
    /// Using Cow to allow both borrowed (from env vars) and owned strings.
    pub pids_limit: Option<Cow<'a, str>>,
    pub memory: Option<Cow<'a, str>>,
    pub cpus: Option<Cow<'a, str>>,
}

/// Build the complete `podman run` argv for `cfg` / `repo` / `payload`.
///
/// See the module-level documentation for the section order and
/// rationale. The function is pure — it does not read the filesystem
/// or environment — so tests can assert the exact argv.
pub fn podman_run_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
    host_env: &HostEnv,
    params: &Params<'_>,
) -> Result<Vec<String>, Error> {
    let root = repo.root.to_string_lossy().into_owned();
    // The multiplexer applies to the INTERACTIVE payload only (cli.md
    // D11): `mysbx run -- CMD` is a one-shot, and wrapping it in a
    // multiplexer would leave the command's output in a pane nobody
    // attaches to. So a `run` argv is byte-identical to a
    // `multiplexer = "none"` one — no payload swap, no `TMUX_TMPDIR`,
    // and none of the socket guards below (they guard the socket of a
    // session this run does not start).
    let mux = if *payload == Payload::Shell {
        cfg.multiplexer
    } else {
        Multiplexer::None
    };
    if mux.starts_a_session() {
        check_mux_socket(&cfg.mounts, &cfg.state_dirs)?;
        if params.mux_entry.is_none() {
            return Err(Error::MultiplexerUnavailable { multiplexer: mux });
        }
    }
    // The display channel applies to EVERY payload form (D18), like
    // on the bwrap backend — a one-shot `run -- CMD` that opens a
    // window needs it just as much as the interactive shell.
    if cfg.display.is_waypipe() {
        check_display_socket(&cfg.mounts, &cfg.state_dirs)?;
        if params.waypipe.is_none() {
            return Err(Error::DisplayUnavailable);
        }
    }

    // 0. podman run with global args — ARGS ONLY, no program name:
    // lib.rs execs `Command::new(MYSBX_PODMAN).args(argv)`, the same
    // convention as bwrap_argv (golden minimal.txt starts with
    // `--clearenv`). A leading `podman` would double the program
    // name and garble podman's flag parsing.
    let mut argv: Vec<String> = Vec::new();
    // Global args: --runtime=runsc, --runtime-flag per flag, then
    // --cgroup-manager when set. The cgroup manager comes from
    // params: omitted when None, the gvisor tier's shape (a rootless
    // run pins cgroupfs via the env defaults in lib.rs, a root run
    // lets podman's own default apply — hardcoding cgroupfs made
    // runsc configure a cgroup it cannot write, bd myconfig-b13).
    argv.push("--runtime=runsc".into());
    for flag in params.runtime_flags {
        argv.extend(["--runtime-flag".into(), flag.clone()]);
    }
    if let Some(manager) = params.cgroup_manager {
        argv.push(format!("--cgroup-manager={manager}"));
    }

    argv.push("run".into());
    argv.push("--replace".into());
    // Stdio wiring (bd myconfig-jho): the run execs podman with
    // mysbx's own stdio, so the container must be attached the same
    // way — `--interactive` forwards stdin (without it podman closes
    // the container's stdin and a shell payload exits 0 on instant
    // EOF, the f13 silent immediate exit), `--tty` allocates a pty
    // for an interactive payload on a terminal. The gvisor tier's
    // attached runs pass the same pair (podman.rs
    // `build_run_args`, non-detach branch).
    if params.interactive {
        argv.push("--interactive".into());
    }
    if params.tty {
        argv.push("--tty".into());
    }

    // 1. container identity
    // Container name: the repo basename PLUS a short hash of the repo
    // root path. The basename alone collides between different repos
    // that share one — and `--replace` then silently kills the sibling
    // session's container. The hash makes the name unique per repo
    // path (the gvisor tier's `repo_id` precedent: a stable digest of
    // the repo path, not its contents — the container of a repo must
    // keep its name across checkouts and rebuilds).
    let container_name = format!(
        "mysbx-{}-{}",
        repo.root
            .file_name()
            .unwrap_or_else(|| OsStr::new("unknown"))
            .to_string_lossy()
            .replace(|c: char| !c.is_alphanumeric(), "-"),
        fnv1a10(&root)
    );
    argv.extend(["--name".into(), container_name]);
    argv.extend(["--hostname".into(), "mysbx".into()]);
    argv.push("--userns=keep-id".into());

    // 2. base isolation
    argv.push("--read-only".into());
    argv.push("--read-only-tmpfs=true".into());
    argv.push("--cap-drop=ALL".into());
    argv.push("--security-opt=no-new-privileges".into());

    // 3. working directory
    argv.extend(["--workdir".into(), root.clone()]);

    // 4. container home: a fresh empty tmpfs at CONTAINER_HOME, the
    // podman shape of bwrap's tmpfs SANDBOX_HOME (config.md D14).
    //
    // The XDG base-dir variables (section 7) point at
    // `.config`/`.cache`/`.local/{state,share}` under this home, but
    // the container root is read-only (`--read-only` +
    // `--read-only-tmpfs=true`, section 2) and podman's bind copy-up
    // materializes only the FIRST path component of a bind — so
    // without a writable home surface the payload shell dies at
    // startup on its very first state write (`mkdir
    // /mysbx-home/.cache': Read-only file system`, the fish errors of
    // bd myconfig-lpl).
    //
    // Ordering is semantic and verified against the pinned engines,
    // both of which mount PARENTS BEFORE CHILDREN regardless of flag
    // order: podman sorts every user mount by destination depth,
    // stable — "Mounts need to be sorted so paths will not cover
    // other paths" (podman 5.8.6 libpod/util.go sortMounts, called
    // from container_internal_common.go) — and runsc re-sorts the
    // OCI spec's mounts the same way ("Sort the mounts so that we
    // don't place children before parents", runsc/boot/vfs.go
    // prepareMounts; it also CREATES missing mountpoints, so the
    // image needs no /mysbx-home). The tmpfs therefore lands BEFORE
    // every bind under it, and the binds land ON TOP: the ro
    // `~/.config/fish` mount and the state-dirs binds seed the tmpfs
    // exactly like bwrap's section-3 tmpfs does. Emitted before the
    // workspace and state binds anyway (this section) so the argv
    // ALSO reads parents-first — the engines' guarantee is relied
    // on, not required, for an argv-ordered run.
    tmpfs_mount(&mut argv, CONTAINER_HOME);
    // The XDG parent tmpfs mounts (bd myconfig-e50): the base-dir
    // variables of section 7 point at `.local/share`/`.local/state`
    // below the home, but runsc's mount preparation CREATES the
    // missing mountpoint dirs of a bind root-owned 0755 — a parent
    // like `.local/share` that exists only to host the
    // `.local/share/opencode` state bind is then NOT creatable-in
    // for the container user, and fish died with EACCES creating
    // `$XDG_DATA_HOME/fish`. Tmpfs mounts at those parents make them
    // writable; nothing pre-exists in the image at either path, so
    // emitting both unconditionally is harmless (an empty tmpfs dir
    // under the home one). `.config` deliberately gets NO tmpfs: the
    // ro host-config seed mount must stay the visibly-read-only
    // surface (see section 7), and `.cache` needs none — no bind
    // lives under it, so the payload creates it on the home tmpfs
    // itself. A `state-dirs` entry naming one of these parents
    // exactly still works: its bind is emitted in section 5a, LATER
    // than these tmpfs mounts, and podman stable-sorts equal-depth
    // user mounts by argv order — the bind wins, persistence is
    // unchanged.
    tmpfs_mount(&mut argv, CONTAINER_DATA_HOME);
    tmpfs_mount(&mut argv, CONTAINER_STATE_HOME);
    // The state-parent tmpfs mounts (bd myconfig-ixz) — the same
    // root-owned-mountpoint failure, generalized from the XDG pair
    // above: state dirs are by design "the writable part of the
    // home" (D15), and their PARENT in the container must be as
    // writable as the state dir itself, because real tools keep
    // runtime files as SIBLINGS of the state dir. The pi
    // integration persists `.pi/agent/sessions` while pi writes
    // `auth.json`, `settings.json`, `trust.json` and `models.json`
    // DIRECTLY into `.pi/agent` (the ro config binds under it —
    // `extensions/`, `agents/`, `prompts/`, `themes/`,
    // `keybindings.json` — are payload-readable TREES that mount
    // fine on top of a writable parent; `.pi/agent` holds pi's
    // runtime file set, no config file that must stay read-only),
    // and runsc creates the missing bind-mountpoint ancestors
    // root-owned 0755 — with the entry's own bind at
    // `.pi/agent/sessions`, the first sibling write died on f13
    // with
    //     models.json error: Availability refresh: EACCES: permission
    //       denied, open '/mysbx-home/.pi/agent/auth.json'
    // right after the e50 fish fix. The mountpoint parent is a
    // vehicle for the engine, not a readable contract — tmpfs it:
    // ONE dest per entry, the entry's PARENT (the DEEPEST state
    // ancestor). The shallower ones (runsc's own root-owned
    // mountpoint chain, e.g. `.pi` for the `.pi/agent` tmpfs) only
    // need x/search — 0755 has it — while the payload's +w lands on
    // the tmpfs'd parent itself. `.config` stays the
    // deliberately-read-only config surface: only a STATE entry's
    // parent is tmpfsed, never a pure-config ro surface. Clone runs
    // drop the state binds but keep these tmpfses, like the e50
    // pair — nothing pre-exists in the image at any of these paths,
    // an empty tmpfs dir is harmless.
    // Emitted like the e50 pair — section 4, before every bind,
    // infrastructure: no hidden-mount or symlinkable-dest check
    // sees a tmpfs dest, and the state binds land on top in both
    // engines' parents-first sorts (the state-dirs nesting
    // validators of config.rs are untouched: no entry may nest
    // inside another, so at most one tmpfs dest per subtree spine).
    for dest in state_parent_tmpfses(&cfg.state_dirs) {
        tmpfs_mount(&mut argv, &dest);
    }

    // 5. workspace bind
    let (workspace_src, workspace_dest, implicit_rw_sources): (String, String, Vec<PathBuf>) =
        match params.workspace {
            Workspace::Live => {
                // Live mode: repo itself, rw, at its real host path
                bind_mount(&mut argv, &root, &root, true);
                for git_dir in &repo.git_dirs {
                    check_git_dir(git_dir, &cfg.git_dirs)?;
                    bind_mount(
                        &mut argv,
                        &git_dir.to_string_lossy(),
                        &git_dir.to_string_lossy(),
                        true,
                    );
                }
                // Worktrees sibling when it exists
                if let Some(worktrees) = &repo.worktrees {
                    bind_mount(
                        &mut argv,
                        &worktrees.to_string_lossy(),
                        &worktrees.to_string_lossy(),
                        true,
                    );
                }
                (
                    root.clone(),
                    root.clone(),
                    std::iter::once(normalize(&root))
                        .chain(
                            repo.git_dirs
                                .iter()
                                .map(|g| normalize(&g.to_string_lossy())),
                        )
                        .chain(
                            repo.worktrees
                                .as_deref()
                                .map(|w| normalize(&w.to_string_lossy()))
                                .into_iter(),
                        )
                        .collect(),
                )
            }
            Workspace::Clone { clone } => {
                // Clone mode: session's clone at repo's path
                bind_mount(&mut argv, &clone.to_string_lossy(), &root, true);
                // Clone is the ONLY writable bind (D4)
                (
                    clone.to_string_lossy().into_owned(),
                    root.clone(),
                    vec![normalize(&clone.to_string_lossy())],
                )
            }
        };

    // 5a. state-dirs binds (config.md D15)
    let state_binds: Vec<(String, String)> = if let Workspace::Clone { .. } = params.workspace {
        Vec::new()
    } else {
        check_state_dirs(&cfg.state_dirs)?;
        cfg.state_dirs
            .iter()
            .map(|entry| {
                (
                    repo.sidecar
                        .join("state")
                        .join(entry)
                        .to_string_lossy()
                        .into_owned(),
                    format!("{CONTAINER_HOME}/{entry}"),
                )
            })
            .collect()
    };
    for (_src, dest) in &state_binds {
        if let Some(protected) = check_dest(dest) {
            return Err(Error::ProtectedDest {
                dest: dest.clone(),
                protected,
            });
        }
    }
    for (src, dest) in &state_binds {
        bind_mount(&mut argv, src, dest, true);
    }

    // 5b. the waypipe socket bind (D18): the per-run host directory
    // holding `waypipe.sock`, bound rw at ITSELF, right after the
    // state binds — the same implicit-infrastructure placement as on
    // the bwrap backend. The directory holds exactly one socket file
    // and nothing else.
    let waypipe_bind: Option<(String, String)> = cfg.display.is_waypipe().then(|| {
        let dir = params
            .waypipe
            .as_ref()
            .expect("checked at the top of the builder")
            .socket_dir
            .to_owned();
        (dir.clone(), dir)
    });
    if let Some((src, dest)) = &waypipe_bind {
        bind_mount(&mut argv, src, dest, true);
    }

    // Review-3 item 3: policy file protection (same as bwrap)
    for src in cfg
        .mounts
        .iter()
        .filter(|m| m.mode == Mode::Rw && matches!(params.workspace, Workspace::Live))
        .map(|m| normalize(&m.path))
        .chain(implicit_rw_sources.into_iter())
    {
        for policy in params.policy_paths {
            for guarded in &policy.guarded {
                let pol = normalize(&guarded.to_string_lossy());
                if pol.starts_with(&src) {
                    return Err(Error::PolicyFileWritable {
                        source: src.to_string_lossy().into_owned(),
                        policy: policy.path.display().to_string(),
                        exposed: guarded.display().to_string(),
                    });
                }
            }
        }
        for (state_src, _dest) in &state_binds {
            let state_src = normalize(state_src);
            if state_src.starts_with(&src) && state_src != src {
                return Err(Error::StateTreeWritable {
                    source: src.to_string_lossy().into_owned(),
                    state_dir: state_src.to_string_lossy().into_owned(),
                });
            }
        }
    }

    // 6. configured mounts
    for m in &cfg.mounts {
        let dest = m.dest.as_deref().unwrap_or(&m.path);
        if let Some(protected) = check_dest(dest) {
            return Err(Error::ProtectedDest {
                dest: dest.to_string(),
                protected,
            });
        }
    }
    // Network check (same as bwrap)
    if !cfg.network {
        const DAEMON_DIR: &str = "/nix/var/nix";
        for src in cfg
            .mounts
            .iter()
            .map(|m| normalize(&m.path))
            .chain(std::iter::once(normalize(&workspace_src)))
        {
            if src.starts_with(DAEMON_DIR) || Path::new(DAEMON_DIR).starts_with(&src) {
                return Err(Error::DaemonUnderDeniedNetwork {
                    source: src.to_string_lossy().into_owned(),
                });
            }
        }
    }
    // Hidden mounts check
    let (implicit_git_dirs, implicit_worktrees): (&[PathBuf], Option<&Path>) =
        match params.workspace {
            Workspace::Live => (&repo.git_dirs, repo.worktrees.as_deref()),
            Workspace::Clone { .. } => (&[], None),
        };
    check_hidden_mounts(
        &cfg.mounts,
        &root,
        implicit_git_dirs,
        implicit_worktrees,
        &state_binds,
        waypipe_bind.as_ref().map(|(s, d)| (s.as_str(), d.as_str())),
    )?;
    check_symlinkable_dests(
        &cfg.mounts,
        &workspace_src,
        &workspace_dest,
        implicit_git_dirs,
        implicit_worktrees,
        &state_binds,
        waypipe_bind.as_ref().map(|(s, d)| (s.as_str(), d.as_str())),
    )?;
    for m in &cfg.mounts {
        // Clone run: force all mounts to read-only (D4)
        let ro = m.mode == Mode::Ro || matches!(params.workspace, Workspace::Clone { .. });
        bind_mount(
            &mut argv,
            &m.path,
            m.dest.as_deref().unwrap_or(&m.path),
            !ro,
        );
    }

    // 7. environment
    for (key, value) in host_env {
        argv.extend(["--env".into(), format!("{key}={value}")]);
    }
    for (key, value) in &cfg.env {
        argv.extend(["--env".into(), format!("{key}={value}")]);
    }
    // Backend pins (MYSBX_GVISOR_ENV): after the layers, so a pin wins
    // over a configured value, and before the infrastructure variables
    // below, which stay the last word on `HOME`/`PATH`/the XDG dirs.
    for entry in params.extra_env {
        argv.extend(["--env".into(), entry.clone()]);
    }
    // Infrastructure variables. `PATH` points INSIDE the image
    // (params.tools_path defaults to the agent image's own
    // `/bin:/usr/bin`), and the CA-bundle variables are NOT set at
    // all: the image pins its own bundle in its OCI env
    // (agent-image.nix sets `SSL_CERT_FILE` & co. at
    // `/etc/ssl/certs/ca-bundle.crt`), and a host store path would
    // not exist inside the container anyway (bd myconfig-wao).
    argv.extend(["--env".into(), format!("HOME={CONTAINER_HOME}")]);
    // The XDG base dirs are infrastructure like `HOME` (config.md D14):
    // they are derived FROM it, and re-anchoring them matters ONLY under
    // this backend — bubblewrap's `HOME` is already its own tmpfs,
    // while here the container root is read-only (`--read-only` +
    // `--read-only-tmpfs=true`, section 2) and podman's bind copy-up
    // materializes only the FIRST path component of a bind. The
    // section-4 tmpfs at the home is what makes every one of the
    // four destinations writable (bd myconfig-lpl); emitting the
    // variables additionally keeps tools off the image's own
    // `/home/agent` paths, which the read-only root leaves as
    // unwritable as the pre-4 home was.
    //
    // Writability comes from the section-4 tmpfs mounts — the home
    // PLUS the two XDG `.local` parents (bd myconfig-e50): runsc
    // would create the bind-mountpoint parents root-owned and the
    // container user could not create siblings under them.
    //
    // The values mirror the gVisor agent image's own OCI env
    // (agent-image.nix), only re-anchored at the mysbx home:
    // `.config` under the read-only host-config mount — a tool that
    // writes there fails with the SAME visible EROFS naming its old
    // `$HOME/<dir>` path, so nothing silently loses state — while
    // `.cache`/`.local` are tmpfs-ephemeral unless a `state-dirs`
    // entry binds a sidecar store over them (D15; nesting rules keep
    // the entries out of each other's binds).
    // Set after the layers, like `HOME` — a later `--env` wins, so no
    // config entry can repoint them.
    argv.extend([
        "--env".into(),
        format!("XDG_CONFIG_HOME={CONTAINER_HOME}/.config"),
    ]);
    argv.extend([
        "--env".into(),
        format!("XDG_CACHE_HOME={CONTAINER_HOME}/.cache"),
    ]);
    argv.extend([
        "--env".into(),
        format!("XDG_STATE_HOME={CONTAINER_STATE_HOME}"),
    ]);
    argv.extend([
        "--env".into(),
        format!("XDG_DATA_HOME={CONTAINER_DATA_HOME}"),
    ]);
    argv.extend(["--env".into(), format!("PATH={}", params.tools_path)]);
    if mux.starts_a_session() {
        argv.extend(["--env".into(), format!("TMUX_TMPDIR={MUX_SOCKET_DIR}")]);
    }
    // `XDG_RUNTIME_DIR` of a `display = "waypipe"` run (D18):
    // infrastructure like `HOME`, anchoring the guest waypipe
    // server's display socket (a bare `WAYLAND_DISPLAY` name is
    // created in the runtime dir). Set last, so no config entry can
    // repoint it. `WAYLAND_DISPLAY` itself is set by the waypipe
    // server for the wrapped payload.
    if cfg.display.is_waypipe() {
        argv.extend(["--env".into(), format!("XDG_RUNTIME_DIR={CONTAINER_HOME}")]);
    }

    // 8. resource limits
    // Only apply resource limits when cgroups are enabled.
    // When ignore_cgroups=true (runtime flag), these are skipped.
    if !params.ignore_cgroups {
        if let Some(limit) = &params.pids_limit {
            argv.extend(["--pids-limit".into(), limit.as_ref().into()]);
        }
        if let Some(mem) = &params.memory {
            argv.extend(["--memory".into(), mem.as_ref().into()]);
        }
        if let Some(cpu) = &params.cpus {
            argv.extend(["--cpus".into(), cpu.as_ref().into()]);
        }
    }

    // 9. network
    // Explicitly set network spec:
    // - None: podman uses its default (shared network)
    // - Some("none"): no network access
    // - Some(pasta_spec): custom pasta networking
    if let Some(spec) = params.network_spec {
        argv.extend(["--network".into(), spec.into()]);
    }

    // 10. image
    argv.push(params.image.into());

    // 11. payload — with NO `--` separator before it: docker strips
    // a `--` between the image and the command, podman does NOT — it
    // passes the token through as the container command's argv[0],
    // and runsc then fails with `error finding executable "--"` (bd
    // myconfig-ivp). The first token after the image IS the command.
    // A `display = "waypipe"` run therefore starts with the in-image
    // waypipe (D18): waypipe's OWN `--` separates its command, so
    // mysbx adds none before the image command. All of its flags are
    // ROOT options in waypipe's CLI (the subcommand must come first),
    // and its multi mode is the one that CREATES the display socket
    // for the payload — and one server-conn child per window the
    // payload opens — and it exits when the payload does.
    if cfg.display.is_waypipe() {
        let wp = params
            .waypipe
            .as_ref()
            .expect("checked at the top of the builder");
        argv.push(wp.guest_bin.into());
        argv.push("--socket".into());
        argv.push(format!("{}/waypipe.sock", wp.socket_dir).into());
        argv.push("--display".into());
        argv.push(WAYPIPE_DISPLAY.into());
        argv.push("server".into());
        argv.push("--".into());
    }
    match payload {
        Payload::Shell if mux.starts_a_session() => {
            argv.push(params.mux_entry.unwrap_or(params.shell).into())
        }
        Payload::Shell => argv.push(params.shell.into()),
        Payload::Command(args) => argv.extend(args.iter().cloned()),
    }

    Ok(argv)
}

/// First 10 hex chars of the FNV-1a 64 hash of `path` — a stable,
/// dependency-free digest for the container-name suffix (the zero-
/// dependency crate's equivalent of the gvisor tier's `sha256sum`-
/// exec'ing `repo_id`; colossally unlikely to collide at 40 bits for
/// the handful of repos one host runs).
fn fnv1a10(path: &str) -> String {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in path.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("{hash:016x}")[..10].to_owned()
}

/// Add a bind mount to the argv.
fn bind_mount(argv: &mut Vec<String>, src: &str, dest: &str, rw: bool) {
    let mode = if rw { "rw" } else { "ro" };
    argv.extend([
        "--mount".into(),
        format!("type=bind,src={src},dst={dest},{mode}"),
    ]);
}

/// Add a tmpfs mount to the argv (section 4): the container home —
/// `--mount` form rather than the `--tmpfs DEST` shorthand so the
/// two mount helpers of this builder stay one flag shape each.
fn tmpfs_mount(argv: &mut Vec<String>, dest: &str) {
    argv.extend(["--mount".into(), format!("type=tmpfs,dst={dest}")]);
}

/// Normalize a path string: resolve `..` components lexically.
fn normalize(path: &str) -> PathBuf {
    let mut components = Vec::new();
    for comp in Path::new(path).components() {
        match comp {
            std::path::Component::ParentDir => {
                components.pop();
            }
            std::path::Component::Normal(_) => {
                components.push(comp);
            }
            _ => {}
        }
    }
    if path.starts_with('/') {
        let mut result = PathBuf::from("/");
        for comp in components {
            result.push(comp);
        }
        result
    } else {
        components.iter().collect()
    }
}

/// Check a mount destination against protected paths.
fn check_dest(dest: &str) -> Option<&'static str> {
    // Protected sandbox paths (from bwrap base table)
    const PROTECTED: &[&str] = &[
        "/",
        "/nix/store",
        "/usr/bin",
        "/proc",
        "/dev",
        "/etc/localtime",
        "/tmp",
        "/run",
        "/bin/sh",
    ];
    let dest_norm = normalize(dest);
    for prot in PROTECTED {
        let prot_path = Path::new(prot);
        // `/` must match EXACTLY: every absolute dest is at-or-below `/`
        // by construction, so a prefix match would refuse every
        // legitimate dest. Other paths match in BOTH directions.
        let hits = if *prot == "/" {
            dest_norm == *Path::new("/")
        } else {
            dest_norm.starts_with(prot_path) || prot_path.starts_with(&dest_norm)
        };
        if hits {
            return Some(*prot);
        }
    }
    // [`CONTAINER_HOME`] is protected in ONE direction only, the
    // same guard bwrap runs for its tmpfs home (review-2 item 5): a
    // dest EQUAL to it would replace the section-4 tmpfs while
    // `HOME` still names it, seeding the home with content no layer
    // declared. Strict descendants stay allowed — seeding dotfiles
    // into the home by pointing a `dest` below it is the documented
    // shape (config.md D14, the `~/.config/fish` baseline mount) and
    // lands ON TOP of the tmpfs. On component boundaries the home's
    // only ancestor is `/`, already refused above, so this check
    // effectively guards the EQUAL case.
    if Path::new(CONTAINER_HOME).starts_with(&dest_norm) {
        return Some(CONTAINER_HOME);
    }
    None
}

/// Check multiplexer socket isolation (same as bwrap).
fn check_mux_socket(mounts: &[Mount], state_dirs: &[String]) -> Result<(), Error> {
    for m in mounts {
        let dest = m.dest.as_deref().unwrap_or(&m.path);
        if dest == MUX_SOCKET_DIR || dest.starts_with(&format!("{MUX_SOCKET_DIR}/")) {
            return Err(Error::MuxSocketDest {
                dest: dest.to_string(),
            });
        }
    }
    for entry in state_dirs {
        if format!("{CONTAINER_HOME}/{entry}") == MUX_SOCKET_DIR
            || format!("{CONTAINER_HOME}/{entry}").starts_with(&format!("{MUX_SOCKET_DIR}/"))
        {
            return Err(Error::MuxSocketPersisted {
                entry: entry.clone(),
            });
        }
    }
    Ok(())
}

/// Check the waypipe display socket isolation of a
/// `display = "waypipe"` run (docs/design/config.md D18), mirroring
/// [`check_mux_socket`]: no mount may land at or below
/// [`WAYPIPE_DISPLAY_PATH`], and no `state-dirs` entry may back it.
fn check_display_socket(mounts: &[Mount], state_dirs: &[String]) -> Result<(), Error> {
    for m in mounts {
        let dest = normalize(m.dest.as_deref().unwrap_or(&m.path));
        if dest == normalize(WAYPIPE_DISPLAY_PATH)
            || dest.starts_with(&format!("{WAYPIPE_DISPLAY_PATH}/"))
            || normalize(WAYPIPE_DISPLAY_PATH).starts_with(&format!("{}/", dest.display()))
        {
            return Err(Error::DisplaySocketDest {
                dest: dest.to_string_lossy().into_owned(),
            });
        }
    }
    for entry in state_dirs {
        let dest = normalize(&format!("{CONTAINER_HOME}/{entry}"));
        if dest == normalize(WAYPIPE_DISPLAY_PATH)
            || dest.starts_with(&format!("{WAYPIPE_DISPLAY_PATH}/"))
            || normalize(WAYPIPE_DISPLAY_PATH).starts_with(&format!("{}/", dest.display()))
        {
            return Err(Error::DisplaySocketPersisted {
                entry: entry.clone(),
            });
        }
    }
    Ok(())
}

/// Check git dir approval (same as bwrap).
fn check_git_dir(git_dir: &Path, approved: &[PathBuf]) -> Result<(), Error> {
    if !approved.iter().any(|a| git_dir.starts_with(a)) {
        return Err(Error::GitDirNotApproved {
            gitdir: git_dir.to_owned(),
        });
    }
    Ok(())
}

/// Check state-dirs nesting (same as bwrap).
fn check_state_dirs(entries: &[String]) -> Result<(), Error> {
    for (i, outer) in entries.iter().enumerate() {
        for inner in entries.iter().skip(i + 1) {
            if inner.starts_with(&format!("{outer}/")) {
                return Err(Error::StateDirNesting {
                    outer: outer.clone(),
                    inner: inner.clone(),
                });
            }
        }
    }
    Ok(())
}

/// Check for hidden mounts (same as bwrap).
fn check_hidden_mounts(
    mounts: &[Mount],
    repo_root: &str,
    git_dirs: &[PathBuf],
    worktrees: Option<&Path>,
    state_binds: &[(String, String)],
    waypipe_bind: Option<(&str, &str)>,
) -> Result<(), Error> {
    // Build list of implicit binds
    let mut implicit: Vec<String> = vec![repo_root.to_string()];
    implicit.extend(git_dirs.iter().map(|g| g.to_string_lossy().into_owned()));
    if let Some(wt) = worktrees {
        implicit.push(wt.to_string_lossy().into_owned());
    }
    implicit.extend(state_binds.iter().map(|(_, d)| d.clone()));
    if let Some((_src, dest)) = waypipe_bind {
        implicit.push(dest.to_string());
    }

    // Check each mount against earlier ones and implicit binds
    let mut all_dests: Vec<String> = implicit.clone();
    for (i, m) in mounts.iter().enumerate() {
        let dest = m.dest.as_deref().unwrap_or(&m.path);
        let dest_norm = normalize(dest);
        // Check against earlier mounts
        for earlier in &all_dests[..i + implicit.len()] {
            let earlier_norm = normalize(earlier);
            if dest_norm.starts_with(&earlier_norm) || earlier_norm.starts_with(&dest_norm) {
                return Err(Error::HiddenMount {
                    message: format!(
                        "mount dest {dest} would hide or be hidden by earlier bind {earlier}"
                    ),
                });
            }
        }
        all_dests.push(dest.to_string());
    }
    Ok(())
}

/// Check for dests below writable binds (same as bwrap).
fn check_symlinkable_dests(
    mounts: &[Mount],
    workspace_src: &str,
    _workspace_dest: &str,
    git_dirs: &[PathBuf],
    worktrees: Option<&Path>,
    state_binds: &[(String, String)],
    waypipe_bind: Option<(&str, &str)>,
) -> Result<(), Error> {
    // Build writable set
    let mut writable: Vec<String> = vec![workspace_src.to_string()];
    writable.extend(git_dirs.iter().map(|g| g.to_string_lossy().into_owned()));
    if let Some(wt) = worktrees {
        writable.push(wt.to_string_lossy().into_owned());
    }
    writable.extend(state_binds.iter().map(|(s, _)| s.clone()));
    if let Some((src, _dest)) = waypipe_bind {
        writable.push(src.to_string());
    }

    for m in mounts {
        let dest = m.dest.as_deref().unwrap_or(&m.path);
        let dest_norm = normalize(dest);
        for w in &writable {
            let w_norm = normalize(w);
            if dest_norm.starts_with(&w_norm) && dest_norm != w_norm {
                return Err(Error::DestBelowWritable {
                    dest: dest.to_string(),
                    writable: w.clone(),
                });
            }
        }
    }
    Ok(())
}

/// Why the argv cannot be built safely.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// The mount's `dest` would shadow or overwrite a protected sandbox path.
    ProtectedDest {
        dest: String,
        protected: &'static str,
    },
    /// A later mount's dest hides an earlier bind.
    HiddenMount { message: String },
    /// A `.git` FILE points at git metadata outside the repo that no trusted layer approved.
    GitDirNotApproved { gitdir: PathBuf },
    /// A mount `dest` lies below a writable bind.
    DestBelowWritable { dest: String, writable: String },
    /// A writable bind would expose a trusted policy file.
    PolicyFileWritable {
        source: String,
        policy: String,
        exposed: String,
    },
    /// A writable bind would expose an ancestor of a `state-dirs` backing store.
    StateTreeWritable { source: String, state_dir: String },
    /// A configured mount would carry the nix daemon into a sandbox whose network is denied.
    DaemonUnderDeniedNetwork { source: String },
    /// Two `state-dirs` entries nest.
    StateDirNesting { outer: String, inner: String },
    /// The configuration selects a multiplexer but this build pinned no entry for it.
    MultiplexerUnavailable { multiplexer: Multiplexer },
    /// A mount `dest` is related to [`MUX_SOCKET_DIR`].
    MuxSocketDest { dest: String },
    /// A `state-dirs` entry would make [`MUX_SOCKET_DIR`] sidecar-backed.
    MuxSocketPersisted { entry: String },
    /// The configuration selects the waypipe display (D18) but this
    /// build pinned no in-image waypipe (`MYSBX_GVISOR_WAYPIPE`). The
    /// container mounts nothing from the host `/nix/store`, so no
    /// host pin could serve — the refusal names the image.
    DisplayUnavailable,
    /// A mount `dest` is related to [`WAYPIPE_DISPLAY_PATH`].
    DisplaySocketDest { dest: String },
    /// A `state-dirs` entry would back [`WAYPIPE_DISPLAY_PATH`] with
    /// a sidecar directory.
    DisplaySocketPersisted { entry: String },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ProtectedDest { dest, protected } => write!(
                f,
                "mount dest {dest} would shadow or overwrite the protected \
                 sandbox path {protected}"
            ),
            Error::HiddenMount { message } => f.write_str(message),
            Error::GitDirNotApproved { gitdir } => {
                write!(f, "git metadata {} is not approved", gitdir.display())
            }
            Error::DestBelowWritable { dest, writable } => {
                write!(f, "mount dest {dest} lies below {writable}")
            }
            Error::PolicyFileWritable {
                source,
                policy,
                exposed,
            } => write!(
                f,
                "source {source} would expose the policy file {policy} \
                 writable (through {exposed})"
            ),
            Error::StateTreeWritable { source, state_dir } => write!(
                f,
                "source {source} would expose the state directory {state_dir}"
            ),
            Error::DaemonUnderDeniedNetwork { source } => write!(
                f,
                "source {source} is inside or above the nix daemon directory \
                 /nix/var/nix, and this sandbox denies the network"
            ),
            Error::StateDirNesting { outer, inner } => {
                write!(f, "state-dirs entries nest: {outer} contains {inner}")
            }
            Error::MultiplexerUnavailable { multiplexer } => write!(
                f,
                "multiplexer {multiplexer} is selected but no entry is pinned \
                 (the container image carries no in-image entry script; \
                 MYSBX_MUX_ENTRY_* names a host store path this backend \
                 mounts nothing of)"
            ),
            Error::MuxSocketDest { dest } => write!(
                f,
                "mount dest {dest} is at or below the multiplexer socket \
                 directory {MUX_SOCKET_DIR}"
            ),
            Error::MuxSocketPersisted { entry } => write!(
                f,
                "state-dirs entry {entry} would persist the multiplexer \
                 socket directory {MUX_SOCKET_DIR}"
            ),
            Error::DisplayUnavailable => write!(
                f,
                "display = \"waypipe\" but no in-image waypipe is pinned \
                 (MYSBX_GVISOR_WAYPIPE) \u{2014} the container would run \
                 headless instead of getting its windows \
                 (docs/design/config.md D18); bake waypipe into the \
                 container image (myconfig.ai.dev.mysbx.display.package), \
                 or set display = \"off\""
            ),
            Error::DisplaySocketDest { dest } => write!(
                f,
                "mount dest {dest} is at or below the waypipe display \
                 socket {WAYPIPE_DISPLAY_PATH}, which the in-container \
                 waypipe server creates inside the container home"
            ),
            Error::DisplaySocketPersisted { entry } => write!(
                f,
                "state-dirs entry {entry} would persist the waypipe \
                 display socket {WAYPIPE_DISPLAY_PATH} in the sidecar"
            ),
        }
    }
}

impl std::error::Error for Error {}
