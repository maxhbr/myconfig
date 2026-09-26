// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The nono backend's grant emitter (backends.md D1: bubblewrap with
//! nono inside).
//!
//! The nono backend is LAYERED (docs/design/backends.md D1, the
//! normative decision): bubblewrap builds the filesystem view exactly
//! as the bubblewrap backend does — the tmpfs `/mysbx-home`, every
//! mount `dest`, the ro/rw binds, the clone bound at the repo path
//! (workspace.md D3), the private `/tmp`, the network namespace — and
//! `nono run` wraps the payload INSIDE that view, adding Landlock,
//! seccomp and its egress proxy. lib.rs therefore dispatches the
//! backend to `MYSBX_BWRAP` and passes the argv this module builds as
//! the inner wrapper of `bwrap::bwrap_argv` (`bwrap::Params::inner`):
//! a layout bubblewrap refuses never gets a grant at all, because the
//! grants are derived from the RESOLVED layout, never from the raw
//! config — a layout bug produces a matching grant, not a divergence.
//!
//! Chain shape (D1):
//!
//! ```text
//! bwrap <the bubblewrap backend's argv, unchanged> \
//!   -- <nono> run --profile <path> <grants> <network flags> \
//!   -- <env> <payload environment> <payload>
//! ```
//!
//! The first cut (bd myconfig-6di.2) ran nono directly on the host
//! tree; it is gone. It could not express any mount `dest`, and every
//! mount the generated user layer contains has one (config.md D14) —
//! hence the refusals it carried (`RemapUnsupported`,
//! `CloneUnsupported`, the lib.rs `--session` refusal) are deleted
//! too: bubblewrap binds the dest and the clone, and the repo-root
//! grant below covers the clone because it is bound AT the repo's
//! path (D3, path identity preserved).
//!
//! This module emits ONLY the `nono run` grant argv — the segment
//! between `--profile` and nono's own `--`. bwrap owns the
//! filesystem view, the read-only structure (the kernel returns
//! `EROFS` through a read-only bind, so a Landlock rw grant never
//! makes a path writable that bubblewrap made read-only — bd
//! myconfig-uay by construction) and the namespace; nono owns the
//! second layer. The argv builder is pure — it canonicalizes nothing
//! and checks no existence (the merge already did that, config.md
//! D8) — so tests can assert the exact argument vector without
//! spawning nono.
//!
//! Sections, in fixed order (D1, "Grants follow the resolved
//! layout"):
//!
//! 1. `run` and its profile arg (`--profile`, from
//!    `MYSBX_NONO_PROFILE` — the mysbx profile STORE PATH the nix
//!    wrapper pins, bd myconfig-6di.4.3; an operator can still point
//!    it at any nono profile name or path — the argv carries whatever
//!    the pin names) — WITHOUT the program name: lib.rs prepends the
//!    nono binary itself, the same convention as the other builders.
//! 2. the workspace, rw + `--allow-cwd` (the nono-app.nix pairing):
//!    the repo root in a live run — or the clone bound AT the repo
//!    path in a clone run, which the same grant covers (workspace.md
//!    D3) — plus, live only, the approved git metadata directories
//!    (bwrap binds them, the approval check runs there too) and the
//!    worktrees sibling.
//! 3. `/mysbx-home`, rw, as ONE grant (D1): the sandbox home is a
//!    writable tmpfs like on the bubblewrap backend (config.md
//!    D14), the state binds below it are rw stores, and the
//!    read-only seeds stay read-only through their read-only binds —
//!    Landlock cannot make a hole inside a writable grant, and here
//!    it does not need to.
//! 4. the configured mounts, in declaration order, at their
//!    in-sandbox dest (the default is the source path): `--allow` or
//!    `--read` per the EFFECTIVE mode at that path — a later bind on
//!    the same dest wins, the same rule bubblewrap applies, and a
//!    clone run downgrades every entry to ro (workspace.md D4). A
//!    dest below `/mysbx-home` needs no grant: the single rw grant of
//!    section 3 covers it (and its read-only bind keeps it ro).
//!    `--read-file`/`--allow-file` for file dests is bd
//!    myconfig-2pv, not yet expressible in the config schema.
//! 5. `--read /nix/store` unconditional (the tier invariant of
//!    nono-app.nix's readOnlyDirFlags): the pinned store paths of
//!    the shell, the tool closure and everything the wrapper baked
//!    must be executable/readable through the view's ro store bind.
//! 6. the nix daemon socket, ONLY in the shared-no-allowlist case:
//!    nono's `--allow-unix-socket` on the default daemon socket path
//!    (the flag implies `--read-file` on the socket) rides with the
//!    bwrap bind of `/nix/var/nix` — both exist exactly when the
//!    network is shared WITHOUT an allowlist (bubblewrap parity, bd
//!    myconfig-nj9). Under an allowlist the socket is bound by
//!    neither layer: the daemon builds fixed-output derivations,
//!    which keep network access, and a filtered sandbox must not
//!    expose a hole the allowlist cannot see.
//! 7. network (the mapping of the D1 table, bd myconfig-6di.4.4):
//!    `--block-net` when `network = false` (defense in depth next to
//!    the empty netns — nono allows outbound by default); NO network
//!    flag when the network is shared without an allowlist (nono
//!    0.74.0's default IS outbound allowed — no mediation, the pure
//!    seccomp baseline applies only under an allowlist); and the
//!    allowlist mapping `--allow-domain`/`--allow-connect-port`/
//!    `--listen-port` per merged entry otherwise (the proxy resolves
//!    DNS itself, so no port 53/853 is added). Two refusals guard
//!    the allowlist's honesty (bd myconfig-a14, bd
//!    myconfig-6di.4.4): `listen-ports` alone does not restrict
//!    outbound traffic (nono reports "outbound allowed" with only
//!    listen ports) — an allowlist that does not restrict is a lie,
//!    so the config is refused; and `allow-domains` entries are
//!    plain host names only — a URL or path form (no matter how it
//!    is spelled) would need nono's TLS interception, out of scope
//!    on this backend.
//! 8. the multiplexer and display refusals, until their tasks land:
//!    a session-starting multiplexer (bd myconfig-6di.4.5 — the
//!    private socket directory exists again inside the sandbox home
//!    tmpfs, but nono's unix-socket grants are not audited yet) and
//!    the waypipe display (bd myconfig-6di.4.6 — its syscall set
//!    needs an audit under nono's seccomp filter). Refused, never
//!    silently headless or session-less.
//!
//! Everything else is the bubblewrap backend's: `bin_sh`, `nix_conf`
//! and `ca_bundle` are argv binds inside the view, the payload
//! environment is applied by the pinned `env` between nono and the
//! payload (bwrap.rs, backends.md D1 "Two environments"), and
//! `/mysbx-nono` — nono's own state tmpfs, never granted — is a base
//! path of the bwrap layout (bwrap.rs [`crate::bwrap::NONO_STATE`]).

use crate::bwrap::{Payload, Workspace, SANDBOX_HOME};
use crate::config::{Mode, Multiplexer};
use crate::merge::Merged;
use crate::repo::Repo;
use std::fmt;
use std::path::Path;

/// The nix daemon socket nono's `--allow-unix-socket` names — the
/// default socket path of the Nix daemon (the path
/// `/nix/var/nix/daemon-socket/socket` that exists on every NixOS
/// host), readable in the view through bwrap's read-only bind of
/// `/nix/var/nix` and connectable through this one flag (it implies
/// `--read-file` on the socket).
pub const NIX_DAEMON_SOCKET: &str = "/nix/var/nix/daemon-socket/socket";

/// CLI-layer facts of the grant emitter, like every other builder's
/// `Params`. The shell, tool closure and policy pins are NOT here:
/// they belong to the bwrap layout (bwrap.rs `Params`), which builds
/// the view this emitter grants into.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Params<'a> {
    /// Which tree this run works in (workspace.md D1): live or a
    /// named session's clone — bwrap binds the clone AT the repo's
    /// own path (D3), so the repo-root grant covers it either way.
    pub workspace: Workspace<'a>,
    /// The nono profile of every `nono run` (`MYSBX_NONO_PROFILE`):
    /// the mysbx profile STORE PATH the nix wrapper pins (bd
    /// myconfig-6di.4.3 — a reviewable policy matching the bwrap
    /// view), or any nono profile name/path an operator picks. The
    /// argv carries whatever the pin names.
    pub profile: &'a str,
}

/// Validate the allowlist's honesty (bd myconfig-a14, bd
/// myconfig-6di.4.4) — the pure predicate both the pipeline's step 4b
/// (lib.rs, BEFORE the session clone, so a broken configuration
/// creates nothing) and the argv builder (defense in depth) call:
/// `listen-ports` alone does not restrict outbound traffic (nono
/// reports "outbound allowed" with only listen ports — an allowlist
/// that does not restrict is a lie), and `allow-domains` entries are
/// plain host names on this backend (a URL/path form would need
/// nono's TLS interception, out of scope).
pub fn validate_allowlist(cfg: &Merged) -> Result<(), Error> {
    if cfg.allow_domains.is_empty() && cfg.connect_ports.is_empty() && !cfg.listen_ports.is_empty()
    {
        return Err(Error::ListenPortsOnly);
    }
    for domain in &cfg.allow_domains {
        if domain.contains("://") || domain.contains('/') {
            return Err(Error::DomainUrlForm {
                domain: domain.clone(),
            });
        }
    }
    Ok(())
}

/// Build the `nono run` grant argv — the segment between `run
/// --profile <path>` and nono's own `--` separator, wrapped by
/// lib.rs into the bwrap layout as [`crate::bwrap::Params::inner`].
///
/// See the module-level documentation for the section order and
/// rationale. The function is pure — it does not read the filesystem
/// or environment — so tests can assert the exact argv.
pub fn nono_run_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
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
    let clone_run = matches!(params.workspace, Workspace::Clone { .. });

    // 1. `run` and its profile arg — ARGS ONLY, no program name:
    // lib.rs prepends the nono binary, the same convention as
    // bwrap_argv and podman_run_argv.
    let mut argv: Vec<String> = vec!["run".into()];
    argv.extend(["--profile".into(), params.profile.into()]);

    // 2. the workspace, rw, plus `--allow-cwd` — the nono-app.nix
    // pairing: nono's own cwd handling treats the working directory
    // as a separate grant from the directory tree it lives in, and
    // bwrap `--chdir`s into the repo root. Live: the repo itself
    // (bwrap binds it rw, config.md D13), the approved git metadata
    // directories (bwrap binds them rw; a repo-writable `.git`
    // pointer is no mount specification, the approval check runs in
    // the bwrap layout too) and the worktrees sibling. Clone: the
    // clone bound AT the repo's own path (workspace.md D3) — the
    // root grant covers it, and neither the git dirs, nor the
    // worktrees sibling, nor any state store is bound or granted.
    argv.extend(["--allow".into(), root.clone()]);
    argv.push("--allow-cwd".into());
    if !clone_run {
        for git_dir in &repo.git_dirs {
            argv.extend(["--allow".into(), git_dir.to_string_lossy().into_owned()]);
        }
        if let Some(worktrees) = &repo.worktrees {
            argv.extend(["--allow".into(), worktrees.to_string_lossy().into_owned()]);
        }
    }

    // 3. `/mysbx-home`, rw, as ONE grant (D1): the sandbox home tmpfs
    // plus the rw state binds below it. The read-only seeds below it
    // stay read-only through their read-only binds (the kernel
    // returns `EROFS` before Landlock is consulted); the private
    // multiplexer socket directory and the waypipe display socket
    // are the same tmpfs (D16–D18), guarded by the refusals below
    // until their tasks land.
    argv.extend(["--allow".into(), SANDBOX_HOME.into()]);

    // 4. the configured mounts at their in-sandbox dest, one grant
    // per distinct dest OUTSIDE `/mysbx-home` (the home grant of
    // section 3 covers everything below it). The mode is the
    // effective mode at that path: a later bind on the same dest
    // wins — the same rule bubblewrap applies — and a clone run
    // downgrades every entry to ro (workspace.md D4). First
    // occurrence fixes the emission order, so a rebind updates the
    // mode in place (deterministic and layout-faithful).
    let mut dests: Vec<(String, Mode)> = Vec::new();
    for m in &cfg.mounts {
        let dest = m.dest.clone().unwrap_or_else(|| m.path.clone());
        let mode = if clone_run { Mode::Ro } else { m.mode };
        match dests.iter_mut().find(|(d, _)| *d == dest) {
            Some(slot) => slot.1 = mode,
            None => dests.push((dest, mode)),
        }
    }
    for (dest, mode) in &dests {
        let dest_norm = crate::bwrap::normalize(dest);
        if dest_norm.starts_with(Path::new(SANDBOX_HOME)) {
            continue; // covered by the /mysbx-home grant
        }
        if *mode == Mode::Rw {
            argv.extend(["--allow".into(), dest.clone()]);
        } else {
            argv.extend(["--read".into(), dest.clone()]);
        }
    }

    // 5. `--read /nix/store` unconditional — the tier invariant of
    // nono-app.nix's readOnlyDirFlags: through bwrap's ro store bind
    // the pinned store paths of the shell, the tool closure and
    // everything the wrapper baked must be executable/readable, or
    // the payload dies before it starts.
    argv.extend(["--read".into(), "/nix/store".into()]);

    // 6. the nix daemon socket, ONLY in the shared-no-allowlist case
    // (bubblewrap parity, bd myconfig-nj9): bwrap binds
    // `/nix/var/nix` ro exactly when the network is shared WITHOUT
    // an allowlist (bwrap.rs threads that same fact), and nono's
    // flag grants connect() on the socket. Under an allowlist the
    // socket is bound by neither layer — the daemon builds
    // fixed-output derivations, which keep network access, and a
    // filtered sandbox must not expose a hole the allowlist cannot
    // see. Under a denied network nothing is bound or granted.
    let has_allowlist = !cfg.allow_domains.is_empty()
        || !cfg.connect_ports.is_empty()
        || !cfg.listen_ports.is_empty();
    if cfg.network && !has_allowlist {
        argv.extend(["--allow-unix-socket".into(), NIX_DAEMON_SOCKET.into()]);
    }

    // 7. network mapping (backends.md D1's table, bd
    // myconfig-6di.4.4): bwrap owns the network namespace (the
    // `--share-net`/empty-netns switch), nono owns the EGRESS filter.
    // nono's default IS outbound allowed (nono 0.74.0; `--allow-net`
    // is deprecated): a shared network without an allowlist is the
    // default, no nono flag — the seccomp-medium baseline hides
    // behind bwrap's namespace, not behind a silently narrow egress
    // list.
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
    } else if has_allowlist {
        // The allowlist, in merged order (merge.rs concatenates user
        // layer first, deduplicated keeping the first occurrence).
        // No `--allow-connect-port 53`/`853` is added: nono's PROXY
        // resolves DNS itself for the allowed domains, and a
        // per-port connect to some resolver is a separate policy the
        // operator can spell out explicitly.
        //
        // Two honesty guards, REFUSED not silently imitated (bd
        // myconfig-a14, bd myconfig-6di.4.4): `validate_allowlist`
        // — the same predicate the pipeline's step 4b runs before
        // the session clone — refuses a listen-ports-only config
        // (with only listen ports nono reports "outbound allowed":
        // an allowlist that does not restrict is a lie) and
        // URL/path-form `allow-domains` entries (no TLS interception
        // on this backend, the D1 notes keep the entries plain host
        // names). Re-run here, defense in depth in a pure function.
        validate_allowlist(cfg)?;
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

    // 8. the multiplexer and display refusals, until their tasks
    // land (bd myconfig-6di.4.5/6di.4.6). The private socket
    // directory of the multiplexer integration lives in the sandbox
    // home tmpfs again (D16/D17 — bwrap builds it), but nono's
    // unix-socket grants are not audited yet; the waypipe display's
    // syscall set (memfd, `SCM_RIGHTS` on the guest-side socket)
    // needs an audit under nono's seccomp filter. Until then the run
    // is refused, never silently session-less or headless.
    if mux.starts_a_session() {
        return Err(Error::MultiplexerUnavailable { multiplexer: mux });
    }
    if cfg.display.is_waypipe() {
        return Err(Error::DisplayUnavailable);
    }

    Ok(argv)
}

/// Why the grant argv cannot be built — the same refusal-only
/// representation as the other backends: a user-reachable
/// configuration that cannot run is an ordinary error (cli.md
/// D8/D9), never a panic. The layout refusals (protected dests,
/// hidden mounts, symlinkable dests, policy files, the state tree,
/// the daemon directory, git-dirs approvals, state-dir nesting) are
/// the bwrap layout's (bwrap::Error): nono never sees a layout
/// bubblewrap would refuse.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// `listen-ports` is configured but neither `allow-domains` nor
    /// `connect-ports`: with only listen ports nono 0.74.0 reports
    /// "outbound allowed" — the listen grant restricts nothing
    /// (bd myconfig-a14, backends.md D1 "an allowlist must restrict
    /// outbound traffic"). Accepted-and-ignored is not an option, so
    /// the configuration is refused: the operator adds a restriction
    /// (`allow-domains`/`connect-ports`) or drops the key.
    ListenPortsOnly,
    /// An `allow-domains` entry in URL or path form (a scheme or a
    /// `/`): nono tunnels plain host names with `CONNECT` and
    /// injects no CA certificate (backends.md D1), so the mysbx CA
    /// pins stay authoritative; a URL form with a path glob needs
    /// nono's TLS interception, out of scope on this backend (the
    /// config.md D21 schema refuses it here, bd myconfig-6di.4.4).
    DomainUrlForm {
        /// The offending entry.
        domain: String,
    },
    /// `network = false` with a non-empty allowlist: the allowlist
    /// contradicts the deny — refused by the pipeline before the
    /// builder ever runs (step 4b, lib.rs), and again here, defense
    /// in depth in a pure function.
    AllowlistUnderDeniedNetwork,
    /// A session-starting multiplexer is selected, but nono's
    /// unix-socket grants for the multiplexer payload are not audited
    /// yet (bd myconfig-6di.4.5): the private socket directory lives
    /// in the sandbox home tmpfs again, and refusing is safer than
    /// assuming the socket passes nono's filters unfiltered.
    MultiplexerUnavailable {
        /// The selected multiplexer.
        multiplexer: Multiplexer,
    },
    /// The waypipe display is selected, but its syscall set (memfd,
    /// `SCM_RIGHTS` on the guest-side socket) needs an audit under
    /// nono's seccomp filter (bd myconfig-6di.4.6) — until that audit
    /// says yes, the run is refused, never silently headless.
    DisplayUnavailable,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ListenPortsOnly => write!(
                f,
                "listen-ports is configured but neither allow-domains nor \
                 connect-ports — with only listen ports nono reports \
                 \"outbound allowed\": the listen grant restricts nothing, \
                 and an allowlist that does not restrict outbound traffic \
                 is a lie (bd myconfig-a14) — add allow-domains or \
                 connect-ports to make the policy real, or drop the key"
            ),
            Error::DomainUrlForm { domain } => write!(
                f,
                "allow-domains entry '{domain}' is a URL or path — on this \
                 backend entries are plain host names (nono tunnels them \
                 with CONNECT and injects no CA certificate, so mysbx's CA \
                 pins stay authoritative); a URL form with a path glob \
                 would need nono's TLS interception, which is out of scope \
                 here (config.md D21, bd myconfig-6di.4.4) — use the bare \
                 host name"
            ),
            Error::AllowlistUnderDeniedNetwork => write!(
                f,
                "network = false denies the network, and an allowlist \
                 (allow-domains/connect-ports/listen-ports) contradicts it — \
                 both cannot hold at once; drop the allowlist or share the \
                 network"
            ),
            Error::MultiplexerUnavailable { multiplexer } => write!(
                f,
                "multiplexer = \"{multiplexer}\" is selected but the nono \
                 backend refuses multiplexer sessions until nono's \
                 unix-socket grants are audited (bd myconfig-6di.4.5) — the \
                 private socket directory lives in the sandbox home tmpfs, \
                 and assuming the socket passes unfiltered is not a claim \
                 mysbx makes; set multiplexer = \"none\", or pick another \
                 backend"
            ),
            Error::DisplayUnavailable => write!(
                f,
                "display = \"waypipe\" but the syscall set it needs (memfd, \
                 SCM_RIGHTS on the guest-side socket) is unaudited under \
                 nono's seccomp filter (docs/design/config.md D18, \"The \
                 other backends\", bd myconfig-6di.4.6) — the run would be \
                 refused by the filter or silently headless, and neither is \
                 acceptable; set display = \"off\", or pick another backend"
            ),
        }
    }
}

impl std::error::Error for Error {}
