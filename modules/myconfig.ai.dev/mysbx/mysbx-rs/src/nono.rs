// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The nono backend argv builder (Landlock + seccomp).
//!
//! Precedent: the nono tier (modules/myconfig.ai.dev/sandboxes/
//! myconfig.ai.nono-agent-sandbox.nix + fns/nono-app.nix, tier 2 of
//! docs/agent-sandboxing-tiers.README.md) runs agents through the
//! `nono` capability sandbox (upstream github.com/nolabs-ai/nono,
//! packaged as pkgs.nono): Landlock for the filesystem and TCP port
//! restrictions (kernel ABI V4+) plus a static seccomp baseline for
//! restricted networking. This backend maps the mysbx merged config
//! (mounts, network policy, env, state-dirs) onto a `nono run`
//! invocation — the THIRD backend, and the first one without any
//! namespace machinery at all.
//!
//! The Landlock model has no path remap and no sandbox home:
//!
//! - NO bind dest != src: Landlock grants access AT a path, it cannot
//!   move a path. A mount with a `dest` that differs from its source
//!   is therefore inexpressible, and a `--session` clone run (the
//!   clone bound AT the repo's path, workspace.md D3) is refused
//!   outright.
//! - NO sandbox home: there is no tmpfs `/mysbx-home` — the host home
//!   stays outside the sandbox's writable set, and `HOME` is set to
//!   the real host home (the payload's writes there fail with EACCES
//!   unless an `--allow` granted a subdirectory).
//! - NO namespaces: no user/pid/net namespace, no pivot_root. The
//!   payload sees the host filesystem exactly; only the Landlock
//!   ruleset decides what is readable and writable.
//!
//! Verified flag surface of `nono run` (nono 0.74.0, `nono run --help`):
//! `run [--profile NAME] [--allow DIR]... [--read DIR]... [--allow-cwd]
//! [--allow-unix-socket SOCK]... [--allow-domain D]...
//! [--allow-connect-port P]... [--listen-port P]... [--block-net]
//! [-- CMD...]`. nono INHERITS the parent environment (there is no
//! `--clearenv` equivalent — the nono-app.nix tier wrapper exports
//! variables before exec'ing nono, the same precedent this backend's
//! exec environment follows). The `default` profile already grants:
//! read of the system dirs (`/bin`, `/usr`, `/lib`, `/etc/ssl`, …),
//! WRITE of `/tmp` and `$TMPDIR` and `/dev/null` & co., DNS and
//! network left allowed by default. Network default under nono is
//! ALLOWED (deny rules are applied per connection) unless `--block-net`;
//! per-domain filtering happens via nono's proxy (`--allow-domain`),
//! per-port via Landlock/seccomp (`--allow-connect-port`,
//! `--listen-port`). No flag exists for "share the whole network
//! namespace", and no path remap flag exists.
//!
//! The argv builder is pure — it canonicalizes nothing and checks no
//! existence (the merge already did that, docs/design/config.md D8) —
//! so tests can assert the exact argument vector without spawning
//! nono.
//!
//! Sections, in fixed order:
//!
//! 1. `run` and its profile arg (`--profile`, from
//!    `MYSBX_NONO_PROFILE`, default "default") — WITHOUT the program
//!    name: lib.rs prepends the backend binary itself via
//!    `Command::new(MYSBX_NONO).envs(exec_env)` (nono inherits the
//!    parent environment; the payload-relevant variables travel in
//!    that exec environment, not in the argv).
//! 2. workspace: `--allow <repo.root>` + `--allow-cwd` (the nono-app
//!    .nix pairing), `--allow` per approved git dir, `--allow` for the
//!    worktrees sibling. A clone run is refused — see
//!    [`Error::CloneUnsupported`].
//! 3. `--read /nix/store` unconditional (the tier invariant: the
//!    pinned store paths of the payload shell and tools must be
//!    executable/readable, nono-app.nix binds exactly this).
//! 4. state-dirs (live mode only): `--allow` per
//!    `<sidecar>/state/<entry>` — at its REAL host path, because
//!    Landlock has no remap: there is no `/mysbx-home/<entry>` dest,
//!    the sandbox sees the sidecar store where the host keeps it.
//! 5. configured mounts, in declaration order: ro → `--read <path>`,
//!    rw → `--allow <path>`; a `dest` != source is refused
//!    ([`Error::RemapUnsupported`]).
//! 6. the policy-file and state-tree refusals (the podman_gvisor.rs
//!    shape of the bwrap checks): an `rw` source that contains a
//!    guarded policy path, or an ancestor of a state backing store,
//!    is refused.
//! 7. the nix daemon socket: `--allow-unix-socket /nix/var/nix/
//!    daemon-socket/socket` when the network is shared; a source at,
//!    below or containing `/nix/var/nix` under a denied network is
//!    refused (the bwrap rule — the socket is a network service).
//! 8. network (THE core mapping, bd myconfig-mo3.1/myconfig-6di.2):
//!    `--block-net` when `network = false` (nono's default is
//!    network-allowed, so the deny must be explicit); an empty
//!    allowlist with a shared network is refused
//!    ([`Error::NetworkSharedUnsupported`]) — nono cannot express
//!    "share the host network"; an allowlist becomes
//!    `--allow-domain`/`--allow-connect-port`/`--listen-port` (the
//!    proxy resolves DNS itself, so no port 53/853 is added).
//! 9. multiplexer and display refusals: a session-starting
//!    multiplexer ([`Error::MultiplexerUnavailable`]) and the waypipe
//!    display ([`Error::DisplayUnavailable`]) are refused — their
//!    socket machinery has no Landlock equivalent in this first cut.
//! 10. `--` then the payload — nono DOES use the `--` separator
//!     (nono-app.nix, `nono run --help`).
//!
//! What deliberately does NOT appear: `bin_sh`, `nix_conf` and
//! `ca_bundle` are NOT argv flags under nono (no bind machinery) —
//! they travel via the exec environment lib.rs builds (`HOME`, `PATH`,
//! `NIX_CONF_DIR`, the CA-bundle variables).

use crate::bwrap::{Payload, PolicyPath, Workspace};
use crate::config::Multiplexer;
use crate::merge::Merged;
use crate::repo::Repo;
use std::collections::BTreeMap;
use std::fmt;
use std::path::{Path, PathBuf};

/// Host environment values the operator chose to forward, keyed by
/// variable name — the same type the other builders take.
pub type HostEnv = BTreeMap<String, String>;

/// The nix daemon socket nono's `--allow-unix-socket` names — the
/// default socket path of the Nix daemon (nix 2.34's own compiled-in
/// default, the path `/nix/var/nix/daemon-socket/socket` that exists
/// on every NixOS host). Under bwrap the whole `/nix/var/nix` tree is
/// bound read-only with the shared network; under nono there is no
/// bind at all — the socket file's readability is implied by
/// `--allow-unix-socket` (the flag documents that it implies
/// `--read-file` on the socket), and the daemon's reachability is
/// exactly this one path.
pub const NIX_DAEMON_SOCKET: &str = "/nix/var/nix/daemon-socket/socket";

/// Common parameters of every invocation that do not come from a
/// configuration layer. Unlike podman_gvisor's `Params` — whose shell
/// and tools live INSIDE a container image — these are HOST paths,
/// like bwrap's: nono runs the payload directly on the host kernel,
/// so the wrapper's own store pins are the payload's userland.
///
/// `bin_sh`, `nix_conf` and `ca_bundle` are NOT fields: nono has no
/// bind machinery, so there is nothing this builder could do with
/// them. They reach the payload through the exec environment lib.rs
/// builds (`PATH` carries the tools closure, `NIX_CONF_DIR` points nix
/// at the pinned file's directory, `SSL_CERT_FILE` & co. name the
/// pinned bundle).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Params<'a> {
    /// Path of the shell used for [`Payload::Shell`] — a host store
    /// path (MYSBX_SHELL), like bwrap's pin: the payload sees it at
    /// its real path through the unconditional `--read /nix/store`.
    pub shell: &'a str,
    /// The dev-tool closure's `bin` directory (MYSBX_TOOLS_PATH) —
    /// set as `PATH` in the exec environment lib.rs builds.
    pub tools_path: &'a str,
    /// The **trusted policy files** this run was configured from,
    /// exactly as `load_layers` read them (see
    /// [`bwrap::PolicyPath`]) — an `--allow` that contains one is
    /// refused, the same refusal the other backends run.
    pub policy_paths: &'a [PolicyPath],
    /// Which tree this run works in (workspace.md D1). Only
    /// [`Workspace::Live`] is supported: a clone run is refused
    /// ([`Error::CloneUnsupported`]) because the clone-remap (clone
    /// bound AT the repo path) is inexpressible under Landlock.
    pub workspace: Workspace<'a>,
    /// The nono profile name (`MYSBX_NONO_PROFILE`, default
    /// "default") — the conservative base profile of the tier
    /// wrapper. A CLI-layer fact like every other field of `Params`.
    pub profile: &'a str,
}

/// Build the complete `nono run` argv for `cfg` / `repo` / `payload`.
///
/// See the module-level documentation for the section order and
/// rationale. The function is pure — it does not read the filesystem
/// or environment — so tests can assert the exact argv.
///
/// `host_env` is part of the shared builder signature (bwrap's
/// `--setenv` and podman's `--env` consume it there); under nono the
/// forwarded variables travel via the exec environment lib.rs
/// builds, so the builder itself ignores the map — the signature
/// stays uniform so callers and tests stay backend-agnostic.
#[allow(unused_variables)]
pub fn nono_run_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
    host_env: &HostEnv,
    params: &Params<'_>,
) -> Result<Vec<String>, Error> {
    let root = repo.root.to_string_lossy().into_owned();
    // The multiplexer applies to the INTERACTIVE payload only
    // (cli.md D11) — the same rule as the other backends: a `run`
    // one-shot never starts a session, so the refusal below sees a
    // `run` payload as `none`.
    let mux = if *payload == Payload::Shell {
        cfg.multiplexer
    } else {
        Multiplexer::None
    };
    // The guards run before anything is pushed so a refused run has
    // produced no partial argv (the builder is pure anyway — the
    // ordering is for the READER of the error, not for side effects).

    // 1. `run` and its profile arg — ARGS ONLY, no program name:
    // lib.rs execs `Command::new(MYSBX_NONO).args(argv)`, the same
    // convention as bwrap_argv and podman_run_argv.
    let mut argv: Vec<String> = vec!["run".into()];
    argv.extend(["--profile".into(), params.profile.into()]);

    // 2. workspace. Live: the repo itself, rw (`--allow`), plus
    // `--allow-cwd` — the exact nono-app.nix pairing, because nono's
    // own cwd handling treats the working directory as a separate
    // grant from the directory tree it lives in. The git metadata
    // dirs are approved-bound like on the other backends
    // (`check_git_dir`), the worktrees sibling is `--allow`ed when it
    // exists.
    match params.workspace {
        Workspace::Live => {
            argv.extend(["--allow".into(), root.clone()]);
            argv.push("--allow-cwd".into());
            for git_dir in &repo.git_dirs {
                check_git_dir(git_dir, &cfg.git_dirs)?;
                argv.extend(["--allow".into(), git_dir.to_string_lossy().into_owned()]);
            }
            if let Some(worktrees) = &repo.worktrees {
                argv.extend(["--allow".into(), worktrees.to_string_lossy().into_owned()]);
            }
        }
        // Clone mode (workspace.md D3) is INEXPRESSIBLE: the whole
        // point of the clone bind is path remap (the clone appears AT
        // the repo's own path while the host repo stays away), and
        // Landlock grants access at a path — it cannot move one. The
        // refusal is the honest outcome; an operator who needs a
        // session clone picks another backend.
        Workspace::Clone { .. } => return Err(Error::CloneUnsupported),
    }

    // 3. `--read /nix/store` unconditional — the tier invariant of
    // nono-app.nix's readOnlyDirFlags: the pinned store paths of the
    // shell, the tool closure and everything the wrapper baked must
    // be executable/readable, or the payload dies before it starts.
    // The store is read-only to the payload on any NixOS host, so
    // this is a read grant, not a widening one.
    argv.extend(["--read".into(), "/nix/store".into()]);

    // 4. state-dirs (config.md D15), live mode only: one `--allow`
    // per declared entry, backed by the sidecar store — at its REAL
    // HOST PATH. This is the semantic difference from the other
    // backends, and it must be said out loud: under bwrap/podman the
    // store is remapped to `/mysbx-home/<entry>` (a dest the payload
    // sees below its sandbox home); under nono there is NO remap, so
    // the payload sees `<repo>.mysbx/state/<entry>` exactly. Tools
    // that key on `$HOME/<entry>` therefore need their `HOME` set to
    // the host home (the exec environment does that) AND the entry
    // resolved against it — which works, because the entry path IS
    // the same string on both sides of the Landlock boundary. The
    // nesting check is the other backends' (no entry inside another:
    // `--allow` is recursive, a nested grant would be redundant, but
    // a nesting here means the CONFIG is ambiguous, and ambiguity is
    // refused like everywhere else).
    let state_allows: Vec<PathBuf> = match params.workspace {
        Workspace::Live => {
            check_state_dirs(&cfg.state_dirs)?;
            cfg.state_dirs
                .iter()
                .map(|entry| repo.sidecar.join("state").join(entry))
                .collect()
        }
        Workspace::Clone { .. } => Vec::new(),
    };
    for store in &state_allows {
        argv.extend(["--allow".into(), store.to_string_lossy().into_owned()]);
    }

    // 5. configured mounts, in declaration order (config.md D7/D8):
    // ro → `--read`, rw → `--allow`, each at its own path. A `dest`
    // that differs from the source is refused — Landlock has no bind
    // remap, and podman_gvisor refused the inexpressible with the
    // same rule (an `rw` mount under a clone run is downgraded there;
    // here the whole mount never gets a chance to be downgraded —
    // nono has no dest at all).
    for m in &cfg.mounts {
        if let Some(dest) = &m.dest {
            if dest != &m.path {
                return Err(Error::RemapUnsupported {
                    path: m.path.clone(),
                    dest: dest.clone(),
                });
            }
        }
        if m.mode == crate::config::Mode::Rw {
            argv.extend(["--allow".into(), m.path.clone()]);
        } else {
            argv.extend(["--read".into(), m.path.clone()]);
        }
    }

    // 6. policy-file protection — the podman_gvisor shape: every
    // implicit rw source (the repo root, the approved git dirs, the
    // worktrees sibling, the state backing stores) plus the `rw`
    // mounts, checked against the guarded paths of the trusted
    // policy files and the state tree. `--allow` is recursive, so a
    // source CONTAINING a guarded path exposes it — the same
    // `starts_with` containment the other backends use.
    let implicit_rw_sources: Vec<PathBuf> = std::iter::once(normalize(&root))
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
        .chain(state_allows.iter().map(|s| normalize(&s.to_string_lossy())))
        .collect();
    let rw_mount_sources: Vec<PathBuf> = cfg
        .mounts
        .iter()
        .filter(|m| m.mode == crate::config::Mode::Rw)
        .map(|m| normalize(&m.path))
        .collect();
    for src in rw_mount_sources.iter().chain(implicit_rw_sources.iter()) {
        for policy in params.policy_paths {
            for guarded in &policy.guarded {
                let pol = normalize(&guarded.to_string_lossy());
                if pol.starts_with(src) {
                    return Err(Error::PolicyFileWritable {
                        source: src.to_string_lossy().into_owned(),
                        policy: policy.path.display().to_string(),
                        exposed: guarded.display().to_string(),
                    });
                }
            }
        }
        for state_src in &state_allows {
            let state_src = normalize(&state_src.to_string_lossy());
            if state_src.starts_with(src) && state_src != *src {
                return Err(Error::StateTreeWritable {
                    source: src.to_string_lossy().into_owned(),
                    state_dir: state_src.to_string_lossy().into_owned(),
                });
            }
        }
    }

    // 7. the nix daemon socket: the one unix socket a shared-network
    // run needs — nono's flag grants connect() on it (and implies
    // `--read-file`, so the socket file itself becomes readable).
    // Under `network = false` the socket is a network service exactly
    // like under bwrap (the daemon builds fixed-output derivations,
    // which keep network access), so a source at, below or containing
    // `/nix/var/nix` is refused with the same rule — there is no
    // `--read /nix/var/nix` bind here at all (that is a bwrap thing);
    // nono's daemon reachability is this socket flag alone.
    if cfg.network {
        argv.extend(["--allow-unix-socket".into(), NIX_DAEMON_SOCKET.into()]);
    } else {
        const DAEMON_DIR: &str = "/nix/var/nix";
        for src in rw_mount_sources.iter().chain(implicit_rw_sources.iter()) {
            if src.starts_with(DAEMON_DIR) || Path::new(DAEMON_DIR).starts_with(src) {
                return Err(Error::DaemonUnderDeniedNetwork {
                    source: src.to_string_lossy().into_owned(),
                });
            }
        }
    }

    // 8. network mapping (bd myconfig-mo3.1, myconfig-6di.2). nono's
    // default is network-ALLOWED: the seccomp baseline denies most
    // connects and the proxy passes the listed domains/ports — an
    // EMPTY allowlist under nono is therefore NOT "the whole
    // internet", but it is NOT "shared network" either, and mysbx's
    // default (`network = true`, no allowlist, bwrap's `--share-net`)
    // would silently become something else. The run is refused; the
    // operator lists what the sandbox may reach or picks a backend
    // that shares.
    let has_allowlist = !cfg.allow_domains.is_empty()
        || !cfg.connect_ports.is_empty()
        || !cfg.listen_ports.is_empty();
    if !cfg.network {
        // The deny must be EXPLICIT under nono (allowed by default),
        // and an allowlist next to it is a contradiction — the
        // pipeline refuses it earlier (step 4b, lib.rs), and the
        // builder refuses it too, defense in depth in a pure
        // function.
        argv.push("--block-net".into());
        if has_allowlist {
            return Err(Error::AllowlistUnderDeniedNetwork);
        }
    } else if !has_allowlist {
        return Err(Error::NetworkSharedUnsupported);
    } else {
        // The allowlist, in merged order (merge.rs concatenates user
        // layer first, deduplicated keeping the first occurrence).
        // No `--allow-connect-port 53`/`853` is added: nono's PROXY
        // resolves DNS itself for the allowed domains, and a
        // per-port connect to some resolver is a separate policy the
        // operator can spell out explicitly.
        for domain in &cfg.allow_domains {
            argv.extend(["--allow-domain".into(), domain.clone()]);
        }
        for port in &cfg.connect_ports {
            argv.extend(["--allow-connect-port".into(), port.to_string()]);
        }
        for port in &cfg.listen_ports {
            argv.extend(["--listen-port".into(), port.to_string()]);
        }
    }

    // 9. multiplexer and display refusals. A session-starting
    // multiplexer: its whole isolation claim on the other backends
    // is the private socket directory inside the sandbox home tmpfs
    // — a path Landlock has no equivalent for (the host tmux socket
    // dir `/tmp/tmux-<uid>` is writable under nono's default profile
    // write grants, so "private" would be a lie). The waypipe
    // display: its syscall set (memfd, `SCM_RIGHTS` on the
    // guest-side socket) needs an audit under nono's seccomp filter
    // — config.md D18's "The other backends" note says exactly this,
    // and until that audit says yes, the run is refused, never
    // silently headless.
    if mux.starts_a_session() {
        return Err(Error::MultiplexerUnavailable { multiplexer: mux });
    }
    if cfg.display.is_waypipe() {
        return Err(Error::DisplayUnavailable);
    }

    // 10. `--` then the payload, verbatim — nono uses the `--`
    // separator (nono-app.nix ends its command with `-- <exe>`).
    argv.push("--".into());
    match payload {
        Payload::Shell => argv.push(params.shell.into()),
        Payload::Command(args) => argv.extend(args.iter().cloned()),
    }

    Ok(argv)
}

/// Normalize a path string: resolve `.`/`..` components lexically —
/// the same helper the other backends carry, because the guarded
/// paths and the mount sources are untrusted config data and the
/// containment checks must see `/a/../b` as `/b`.
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
            std::path::Component::CurDir => {}
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

/// Check git dir approval — the bwrap/podman rule (review-2 item 1):
/// a repo-writable `.git` pointer must not become a grant
/// specification, every target must be at or below an entry of
/// `cfg.git_dirs` (the approval list of the trusted layers).
fn check_git_dir(git_dir: &Path, approved: &[PathBuf]) -> Result<(), Error> {
    if !approved.iter().any(|a| git_dir.starts_with(a)) {
        return Err(Error::GitDirNotApproved {
            gitdir: git_dir.to_owned(),
        });
    }
    Ok(())
}

/// Check state-dirs nesting (config.md D15) — the same check as the
/// other backends: `--allow` is recursive, so a nested entry is
/// redundant at best and an ambiguous config at worst.
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

/// Why the argv cannot be built safely — the same refusal-only
/// representation as the other backends: a user-reachable
/// configuration that cannot be laid out is an ordinary error
/// (cli.md D8/D9), never a panic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// A configured mount has a `dest` different from its source —
    /// Landlock has no bind remap, so the redirect is inexpressible.
    RemapUnsupported {
        /// The mount's source path.
        path: String,
        /// The mount's refused destination.
        dest: String,
    },
    /// A `--session` clone run: the clone-remap (clone bound AT the
    /// repo's own path, workspace.md D3) is inexpressible under
    /// Landlock — there is no way to make the clone appear at the
    /// repo path while keeping the host repo away.
    CloneUnsupported,
    /// A writable grant would expose a trusted policy file — the
    /// same refusal as the other backends (review-3 item 3): a config
    /// the sandbox can write steers the NEXT run of itself.
    PolicyFileWritable {
        /// The grant source the policy file lies below.
        source: String,
        /// The policy file that would become writable.
        policy: String,
        /// The guarded path actually exposed.
        exposed: String,
    },
    /// A writable grant would expose an ancestor of a `state-dirs`
    /// backing store (config.md D15) — the payload could swap a
    /// component for a symlink and redirect the next run's state
    /// bind out of the sidecar.
    StateTreeWritable {
        /// The grant source the backing store lies below.
        source: String,
        /// The state backing store that would become replaceable.
        state_dir: String,
    },
    /// A source is inside or above the nix daemon directory under a
    /// denied network (review-3 item 2): the daemon builds
    /// fixed-output derivations, which keep network access, so a
    /// grant exposing its socket would hand back exactly what
    /// `network = false` takes away.
    DaemonUnderDeniedNetwork {
        /// The offending grant source.
        source: String,
    },
    /// The `.git` FILE points at git metadata no trusted layer
    /// approved (review-2 item 1) — the grant is refused.
    GitDirNotApproved {
        /// The unapproved git metadata directory.
        gitdir: PathBuf,
    },
    /// Two `state-dirs` entries nest (config.md D15).
    StateDirNesting {
        /// The entry declared first (the ancestor).
        outer: String,
        /// The entry declared later (the descendant).
        inner: String,
    },
    /// `network = true` (the mysbx default) with an empty allowlist:
    /// nono cannot express "share the host network" — it mediates
    /// per connection (the seccomp baseline denies most connects;
    /// only listed domains/ports pass). A silent downgrade to
    /// "nothing reachable" would be discovered only when the payload
    /// fails at its first connect, so the run is refused and the
    /// operator decides: list what the sandbox may reach
    /// (`allow-domains`/`connect-ports`/`listen-ports`) or pick a
    /// backend that shares the network.
    NetworkSharedUnsupported,
    /// `network = false` with a non-empty allowlist: the allowlist
    /// contradicts the deny — refused by the pipeline before the
    /// builder ever runs (step 4b, lib.rs), and again here, defense
    /// in depth in a pure function.
    AllowlistUnderDeniedNetwork,
    /// A session-starting multiplexer is selected, but this backend
    /// has no equivalent for the private socket directory: the whole
    /// isolation claim of the multiplexer integration on the other
    /// backends (config.md D16/D17) is that the socket lives in the
    /// sandbox home tmpfs, and under Landlock there is no tmpfs home
    /// to keep it in.
    MultiplexerUnavailable {
        /// The selected multiplexer.
        multiplexer: Multiplexer,
    },
    /// The waypipe display is selected, but its syscall set (memfd,
    /// `SCM_RIGHTS` on the guest-side socket) needs an audit under
    /// nono's seccomp filter (config.md D18's "The other backends"
    /// note) — until that audit says yes, the run is refused, never
    /// silently headless.
    DisplayUnavailable,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::RemapUnsupported { path, dest } => write!(
                f,
                "mount of {path} at {dest} cannot be built \u{2014} nono's Landlock \
                 grants access AT a path, it cannot move one, so a bind dest \
                 different from the source is inexpressible; drop the `dest` \
                 or pick another backend"
            ),
            Error::CloneUnsupported => write!(
                f,
                "session clones are unsupported under nono \u{2014} the clone \
                 must appear AT the repo's own path while the host repo stays \
                 away (docs/design/workspace.md D3), and Landlock cannot move \
                 a path; run live, or pick another backend for --session"
            ),
            Error::PolicyFileWritable {
                source,
                policy,
                exposed,
            } => write!(
                f,
                "source {source} would expose the policy file {policy} \
                 writable (through {exposed}) \u{2014} a config the sandbox can \
                 write, or whose pathname it can re-point, steers the NEXT \
                 run of itself (git-dirs approvals, .git pointers); narrow \
                 the mount to below it, or drop it"
            ),
            Error::StateTreeWritable { source, state_dir } => write!(
                f,
                "source {source} would expose the state directory {state_dir} \
                 to the payload through a writable PARENT \u{2014} the sandbox could \
                 then replace it with a symlink and redirect the next run's \
                 state grant out of the sidecar (docs/design/config.md D15); \
                 mount it read-only, or narrow the grant to the state \
                 directory itself"
            ),
            Error::DaemonUnderDeniedNetwork { source } => write!(
                f,
                "source {source} is inside or above the nix daemon directory \
                 /nix/var/nix, and this sandbox denies the network \u{2014} the daemon \
                 builds fixed-output derivations, which keep network access, so a \
                 grant exposing the socket would hand back exactly what \
                 `network = false` takes away; drop the grant or share the network"
            ),
            Error::GitDirNotApproved { gitdir } => write!(
                f,
                "git metadata {} is not approved \u{2014} a repo-writable .git \
                 file must not become a grant specification (review-2 item 1); \
                 approve the directory in `git-dirs` in the user config or \
                 sidecar (docs/design/config.md D8), or drop the pointer",
                gitdir.display()
            ),
            Error::StateDirNesting { outer, inner } => write!(
                f,
                "state-dirs entries nest: `{inner}` is below `{outer}` \u{2014} \
                 nono's --allow is recursive, so the inner entry is redundant \
                 at best and an ambiguous layout at worst; declare only the \
                 narrowest entry (docs/design/config.md D15)"
            ),
            Error::NetworkSharedUnsupported => write!(
                f,
                "network = true (the default) but no allow-domains/connect-ports/\
                 listen-ports are configured \u{2014} nono cannot share the host \
                 network namespace: it mediates per connection (the seccomp \
                 baseline denies most connects, only listed domains and ports \
                 pass). A run would silently downgrade the mysbx default into \
                 \"almost nothing reachable\", so it is refused; list what the \
                 sandbox may reach, or pick a backend that shares the network"
            ),
            Error::AllowlistUnderDeniedNetwork => write!(
                f,
                "network = false denies the network, and an allowlist \
                 (allow-domains/connect-ports/listen-ports) contradicts it \u{2014} \
                 both cannot hold at once; drop the allowlist or share the \
                 network"
            ),
            Error::MultiplexerUnavailable { multiplexer } => write!(
                f,
                "multiplexer = \"{multiplexer}\" is selected but nono has no \
                 equivalent for the private socket directory \u{2014} the \
                 isolation claim of the multiplexer integration is the socket \
                 inside the sandbox home tmpfs (docs/design/config.md D16/D17), \
                 and under Landlock there is no tmpfs home to keep it in; set \
                 multiplexer = \"none\", or pick another backend"
            ),
            Error::DisplayUnavailable => write!(
                f,
                "display = \"waypipe\" but the syscall set it needs (memfd, \
                 SCM_RIGHTS on the guest-side socket) is unaudited under \
                 nono's seccomp filter (docs/design/config.md D18, \"The \
                 other backends\") \u{2014} the run would be refused by the \
                 filter or silently headless, and neither is acceptable; set \
                 display = \"off\", or pick another backend"
            ),
        }
    }
}

impl std::error::Error for Error {}
