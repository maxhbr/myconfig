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
//!    A single-FILE source grants `--allow-file`/`--read-file` (bd
//!    myconfig-2pv): the kind of the resolved source travels in the
//!    merged config (`Mount::file`, set by the merge's
//!    canonicalization, config.md D8) — nono refuses a directory
//!    grant on a file path with "path … is not a directory".
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
//! 8. the WAYPIPE display channel (bd myconfig-6di.4.6, audited and
//!    LIFTED): the guest waypipe server binds its fake compositor
//!    socket at `/mysbx-home/wayland-0` ([`WAYPIPE_DISPLAY_PATH`], a
//!    direct child of the sandbox home) and connects to the host
//!    client's per-run socket at `<socket_dir>/waypipe.sock` (bwrap
//!    binds the socket dir rw at itself). Both sockets cross nono's
//!    Landlock axes: the FILESYSTEM side is the home grant plus the
//!    socket-dir rw bind, the UNIX-SOCKET side gets two
//!    `--allow-unix-socket-dir-bind` grants (nono 0.74.0 — direct
//!    child sockets only). Audit result (0.74.0 source + live probe
//!    under the real profile + mediation): the TCP-only/block-all
//!    static baselines' socket traps allow AF_UNIX socket() and
//!    socketpair() ([`sandbox/linux.rs`] filter tables), the
//!    AF_UNIX mediation filter explicitly CONTINUES sendmsg with a
//!    NULL msg_name (the fd-passing/SCM_RIGHTS case), memfd_create is
//!    trapped by NO nono filter, and the mediation supervisor allows
//!    bind/connect on granted paths — a waypipe server payload
//!    (bind `wayland-0`, connect `waypipe.sock`, forward) runs
//!    end-to-end under `--block-net` WITH the grants and fails
//!    exactly-when-ungranted (`bind ... (no matching unix_socket
//!    capability)`); under an allowlist or the shared default the
//!    same grants apply (the mediation filter installs in EVERY mode
//!    when the profile sets `af_unix_mediation = "pathname"`, which
//!    is the only seccomp filter that ever touches waypipe's own
//!    syscall set; the shared default has none at all). An operator
//!    overriding `MYSBX_NONO_PROFILE` with a mediation-less profile
//!    gets nono's plain semantics — sockets reachable through the
//!    filesystem grants already given; that is the operator's own
//!    pinned policy, reviewed like every other profile value.
//!
//! Everything else is the bubblewrap backend's: `bin_sh`, `nix_conf`
//! and `ca_bundle` are argv binds inside the view, the payload
//! environment is applied by the pinned `env` between nono and the
//! payload (bwrap.rs, backends.md D1 "Two environments"), and
//! `/mysbx-nono` — nono's own state tmpfs, never granted — is a base
//! path of the bwrap layout (bwrap.rs [`crate::bwrap::NONO_STATE`]).

use crate::bwrap::{Payload, Workspace, SANDBOX_HOME, WAYPIPE_DISPLAY_PATH};
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
    /// The per-run waypipe socket directory (bwrap.rs
    /// [`crate::bwrap::Waypipe::socket_dir`]) when the run opens the
    /// display channel: the guest server CONNECTS to
    /// `<socket_dir>/waypipe.sock`, so the directory needs its
    /// `--allow-unix-socket-dir-bind` grant (bd myconfig-6di.4.6).
    /// `None` when the display is off or when this call runs before
    /// the socket dir exists (the bwrap layout refuses the run first).
    pub waypipe_socket_dir: Option<&'a str>,
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
    // are the same tmpfs (D16–D18).
    argv.extend(["--allow".into(), SANDBOX_HOME.into()]);

    // 3a. the WAYPIPE display's unix-socket grants (bd myconfig-6di.4.6,
    // audited and lifted): the guest waypipe server creates its fake
    // compositor socket at `/mysbx-home/wayland-0` — a DIRECT CHILD of
    // the sandbox home — and connects to the host client's socket at
    // `<socket_dir>/waypipe.sock` — a direct child of the per-run
    // socket dir, bound rw at ITSELF by the layout. Both are
    // `--allow-unix-socket-dir-bind` grants: connect AND bind on
    // direct-child sockets only (nono 0.74.0; audited live under the
    // real profile — see the module docs, section 8). The filesystem
    // side: the home grant above, the socket dir's ro bind below;
    // neither layer widens anything — every granted path lies inside
    // the view bwrap already created.
    if let Some(waypipe_dir) = params.waypipe_socket_dir {
        argv.extend([
            "--allow-unix-socket-dir-bind".into(),
            WAYPIPE_DISPLAY_PATH.into(),
        ]);
        argv.extend([
            "--allow-unix-socket-dir-bind".into(),
            (*waypipe_dir).to_owned(),
        ]);
    }

    // 3b. the multiplexer's UNIX-SOCKET grants (bd myconfig-6di.4.5:
    // the mechanism; bd myconfig-7ov: the per-multiplexer shape the
    // f13 fatal exposed). The socket dirs are inside the home tmpfs —
    // the filesystem side is the rw grant above — but pathname
    // AF_UNIX connect() and bind() are a SEPARATE Landlock axis when
    // the profile asks for mediation (it ships
    // `linux.af_unix_mediation = "pathname"`, see nix/mysbx.nix):
    // without the explicit socket grant the server inside could not
    // create nor connect its own socket, and the first live f13 run
    // (herdr) died with `bind ... (no matching unix_socket
    // capability)` on its API socket. The flag is nono 0.74.0's
    // `--allow-unix-socket-dir-bind`: connect + bind on any
    // DIRECT-CHILD socket of the directory, verified live (a payload
    // making a static bind(2)/connect(2) probe succeeds with the grant
    // and fails both without it). WHICH directories ride with the
    // selected entry is [`Multiplexer::unix_socket_dirs`] — tmux lines
    // up with the $TMUX_TMPDIR socket dir all tracks share, herdr's
    // API sockets live under `$HOME/.config/herdr` (the 4.5 emission
    // of the tmux dir unconditionally was the bug). The
    // `check_mux_socket`/`MuxSocketDest`/`MuxSocketPersisted` guards
    // stay untouched: they protect the TMUX_TMPDIR path itself, not
    // the grants cone. Identifier grants only — every dir is below the
    // home grant, never a hole into the host.
    if mux.starts_a_session() {
        for dir in mux.unix_socket_dirs() {
            argv.extend(["--allow-unix-socket-dir-bind".into(), (*dir).into()]);
        }
    }

    // 4. the configured mounts at their in-sandbox dest, one grant
    // per distinct dest OUTSIDE `/mysbx-home` (the home grant of
    // section 3 covers everything below it). The mode is the
    // effective mode at that path: a later bind on the same dest
    // wins — the same rule bubblewrap applies — and a clone run
    // downgrades every entry to ro (workspace.md D4). First
    // occurrence fixes the emission order, so a rebind updates the
    // mode in place (deterministic and layout-faithful). The kind is
    // tracked alongside the mode (bd myconfig-2pv): a single-file
    // source grants `--allow-file`/`--read-file` — a directory grant
    // on a file path is refused by nono (`path ... is not a
    // directory`, verified against 0.74.0); the LAST bind at a dest
    // decides the kind, exactly like the mode.
    let mut dests: Vec<(String, Mode, bool)> = Vec::new();
    for m in &cfg.mounts {
        let dest = m.dest.clone().unwrap_or_else(|| m.path.clone());
        let mode = if clone_run { Mode::Ro } else { m.mode };
        match dests.iter_mut().find(|(d, _, _)| *d == dest) {
            Some(slot) => {
                slot.1 = mode;
                slot.2 = m.file;
            }
            None => dests.push((dest, mode, m.file)),
        }
    }
    for (dest, mode, file) in &dests {
        let dest_norm = crate::bwrap::normalize(dest);
        if dest_norm.starts_with(Path::new(SANDBOX_HOME)) {
            continue; // covered by the /mysbx-home grant
        }
        match (mode, file) {
            (Mode::Rw, false) => argv.extend(["--allow".into(), dest.clone()]),
            (Mode::Rw, true) => argv.extend(["--allow-file".into(), dest.clone()]),
            (Mode::Ro, false) => argv.extend(["--read".into(), dest.clone()]),
            (Mode::Ro, true) => argv.extend(["--read-file".into(), dest.clone()]),
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

    // The waypipe refusal of the first cut is LIFTED (bd
    // myconfig-6di.4.6, sections 3a and the module docs' audit note):
    // bwrap builds the socket binds, nono grants the two socket dirs,
    // and the profile's AF_UNIX mediation keeps every other pathname
    // socket out. The guest-binary pin requirement is the bwrap
    // layout's (bwrap.rs `Params::waypipe`: `None` is a refused run
    // THERE).

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
        }
    }
}

impl std::error::Error for Error {}
