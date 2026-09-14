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
//! 4. workspace bind: the repo (or clone) mounted at its own path
//!    (config.md D13, workspace.md D3), plus git metadata dirs when
//!    approved, plus the worktrees sibling when it exists, plus
//!    state-dirs binds (config.md D15)
//! 5. configured mounts, in declaration order (config.md D7/D8),
//!    `--mount type=bind,src=HOST,dst=DEST,ro|rw`
//! 6. environment: host-forwarded first, then `cfg.env`, then
//!    infrastructure variables (`HOME`, `PATH`, CA-bundle vars,
//!    `TMUX_TMPDIR` for a multiplexer session)
//! 7. resource limits: `--pids-limit`, `--memory`, `--cpus` (when
//!    cgroups are not ignored)
//! 8. network: `--network` spec (shared by default, or `none` / pasta
//!    spec when `network = false` or configured)
//! 9. image reference (from config or default)
//! 10. payload — WITHOUT a `--` separator before it: the first token
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

use crate::bwrap::{Payload, PolicyPath, Workspace};
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
/// A path **inside** the container, not a host path: the container
/// image provides the home directory structure, and we set `HOME` to
/// it via `--env`. The path deliberately lives outside `/home`, so
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
    /// Which tree this run works in (workspace.md D1): the live repo
    /// (the default, unchanged) or a named session's clone (D3/D4 —
    /// the argv differences are exactly the workspace's). A
    /// CLI-layer fact, like every other field of `Params`: it does
    /// not come from a configuration layer, and no TOML key can name
    /// it (workspace.md D1).
    pub workspace: Workspace<'a>,
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

    // 1. podman run with global args — ARGS ONLY, no program name:
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
    // For interactive runs we'd add --tty --interactive, but for now
    // we build the argv for a generic run (detached vs interactive is
    // a runtime decision, not an argv-builder one)

    // 2. container identity
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

    // 3. base isolation
    argv.push("--read-only".into());
    argv.push("--read-only-tmpfs=true".into());
    argv.push("--cap-drop=ALL".into());
    argv.push("--security-opt=no-new-privileges".into());

    // 4. working directory
    argv.extend(["--workdir".into(), root.clone()]);

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
    )?;
    check_symlinkable_dests(
        &cfg.mounts,
        &workspace_src,
        &workspace_dest,
        implicit_git_dirs,
        implicit_worktrees,
        &state_binds,
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
    // Infrastructure variables. `PATH` points INSIDE the image
    // (params.tools_path defaults to the agent image's own
    // `/bin:/usr/bin`), and the CA-bundle variables are NOT set at
    // all: the image pins its own bundle in its OCI env
    // (agent-image.nix sets `SSL_CERT_FILE` & co. at
    // `/etc/ssl/certs/ca-bundle.crt`), and a host store path would
    // not exist inside the container anyway (bd myconfig-wao).
    argv.extend(["--env".into(), format!("HOME={CONTAINER_HOME}")]);
    argv.extend(["--env".into(), format!("PATH={}", params.tools_path)]);
    if mux.starts_a_session() {
        argv.extend(["--env".into(), format!("TMUX_TMPDIR={MUX_SOCKET_DIR}")]);
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
) -> Result<(), Error> {
    // Build list of implicit binds
    let mut implicit: Vec<String> = vec![repo_root.to_string()];
    implicit.extend(git_dirs.iter().map(|g| g.to_string_lossy().into_owned()));
    if let Some(wt) = worktrees {
        implicit.push(wt.to_string_lossy().into_owned());
    }
    implicit.extend(state_binds.iter().map(|(_, d)| d.clone()));

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
) -> Result<(), Error> {
    // Build writable set
    let mut writable: Vec<String> = vec![workspace_src.to_string()];
    writable.extend(git_dirs.iter().map(|g| g.to_string_lossy().into_owned()));
    if let Some(wt) = worktrees {
        writable.push(wt.to_string_lossy().into_owned());
    }
    writable.extend(state_binds.iter().map(|(s, _)| s.clone()));

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
        }
    }
}

impl std::error::Error for Error {}
