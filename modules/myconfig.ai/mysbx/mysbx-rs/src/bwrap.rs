// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The bubblewrap argv (docs/TODOs/mvp-4-bwrap-argv.md).
//!
//! One pure function and one payload type — no I/O, no environment reads,
//! no path canonicalization (the merge already did that, docs/design/
//! config.md D8). [`bwrap_argv`] is the **single source** of every bwrap
//! invocation: item 5 (the CLI `--dry-run`) prints this vector and item 6
//! executes it, and nothing else may construct a `bwrap` call.
//!
//! This item carries the MVP's security claim
//! (docs/TODOs/mvp-4-bwrap-argv.md): review every decision here against the
//! base table in `docs/plan.md` ("The base"). The argv order is semantic —
//! bubblewrap applies binds in order, so a later narrower bind wins over an
//! earlier wider one and a refactor must not reorder sections 3–5
//! ("Watch out" in the spec). Nesting a later bind inside an earlier
//! one is nevertheless refused when the outer content is writable —
//! see [`check_symlinkable_dests`].

use crate::config::{Mode, Mount};
use crate::merge::Merged;
use crate::repo::Repo;
use std::collections::BTreeMap;
use std::fmt;
use std::path::{Component, Path, PathBuf};

/// What runs inside the sandbox. Either the interactive shell or a command
/// vector (docs/design/cli.md D4: everything after `--` is passed verbatim
/// and never parsed — flag-looking arguments are payload content, not
/// mysbx options).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Payload {
    /// The interactive shell. The shell path is injected (the MVP uses
    /// `bash` from its own closure, not the host `$SHELL` — docs/plan.md,
    /// "Payload shell"), so the caller hands it in; this type stays pure.
    Shell,
    /// An explicit command, verbatim.
    Command(Vec<String>),
}

/// Host environment values the operator chose to forward, keyed by
/// variable name. The builder is pure, so it cannot read `std::env`
/// itself; item 5 collects the forwarded host variables (`TERM COLORTERM
/// LANG LC_ALL EDITOR VISUAL`, each only when set) into this map.
pub type HostEnv = BTreeMap<String, String>;

/// The sandbox's own home directory (docs/design/config.md D14, the
/// `$HOME` row of the base table in docs/plan.md).
///
/// A fresh tmpfs, created by [`base_binds`] and exported as `HOME` — the
/// HOST home is still not mounted, and its *value* is never forwarded
/// either (it is not in `FORWARDED_ENV_VARS`). The path deliberately
/// lives outside `/home`, so nothing inside the sandbox can be confused
/// with a host home path: the invariant "no IN-SANDBOX path under
/// `/home/`" (config.md D14) stays literally checkable on the argv's
/// destinations — mount *sources* are host paths and may of course live
/// in the host home.
pub const SANDBOX_HOME: &str = "/mysbx-home";

/// Common parameters of every invocation that do not come from a
/// configuration layer: the shell binary and the dev-tool `PATH` closure
/// root, both host paths the MVP carries in its own closure
/// (docs/plan.md: "Payload shell", "dev-tool closure on `PATH`").
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Params<'a> {
    /// Path of the shell used for [`Payload::Shell`].
    pub shell: &'a str,
    /// The dev-tool closure's `bin` directory, set as `PATH` inside the
    /// sandbox (git, ripgrep, fd, jq, nix, python3, coreutils, …).
    pub tools_path: &'a str,
    /// A **sanitized** `nix.conf` to bind at `/etc/nix/nix.conf`, or
    /// `None` for no nix configuration at all (review-2 item 3).
    ///
    /// The host's own `/etc/nix/nix.conf` is deliberately never bound:
    /// it may carry `access-tokens` (GitHub/GitLab credentials) and
    /// other secrets, and a read-only bind hands them to the payload
    /// just the same. The Nix wrapper generates a minimal client
    /// configuration instead and pins it here; an unwrapped build
    /// passes `None` and the sandbox runs `nix` with its built-in
    /// defaults.
    pub nix_conf: Option<&'a str>,
    /// Paths of the **trusted policy files** this run was configured
    /// from — the user config and the sidecar config, exactly as
    /// `load_layers` read them. Empty when a layer was absent (an
    /// absent file grants nothing and needs no protection).
    ///
    /// The payload must never be able to write these: the user config
    /// is the host-wide grant layer and the sidecar is the one file a
    /// repository's sandbox runs are steered by, and a writable policy
    /// file turns the NEXT run into a widened one — a `git-dirs`
    /// approval can be added by the attacker, and the `.git` pointer
    /// rewritten to match (review-3 item 3). `rw` mount sources that
    /// contain one are therefore refused; the check also covers the
    /// repo bind itself, which is `rw` by definition.
    pub policy_paths: &'a [PathBuf],
}

/// Build the complete `bwrap` argv for `cfg` / `repo` / `payload`.
///
/// Sections, in this fixed order (order is semantic for overlapping
/// binds — do not reorder):
///
/// 1. `--clearenv`
/// 2. `--unshare-all`, then `--share-net` unless `cfg.network` is `false`
///    (the spec's `network = false` row: "network = false adds
///    `--unshare-net`" is realised as *absence of the re-share*, so
///    either direction is assertable in the golden tests)
/// 3. the base binds (the "The base" table of docs/plan.md):
///    `/nix/store` ro, `/usr/bin` ro, `--proc /proc`, `--dev /dev`,
///    `/etc/localtime` ro, tmpfs `/tmp`, tmpfs [`SANDBOX_HOME`]
/// 4. the repo itself, read-write, at its real host path
///    (docs/design/config.md D13), followed by the git metadata
///    directories its `.git` FILE points at, also rw (review-1 finding 4:
///    worktrees and submodules are unusable without them), then the
///    `state-dirs` binds (config.md D15): each declared entry backed
///    by `<sidecar>/state/<entry>` and bound rw at `/mysbx-home/<entry>`
/// 5. the configured mounts, in declaration order, `--ro-bind` / `--bind`,
///    each `dest` defaulting to its source path (mount order is argv
///    order). Two layout rules are enforced: a dest that would shadow
///    or overwrite a base path — via `..` components or as an ancestor
///    or descendant of one — is refused (see [`check_dest`]); a later
///    dest that would hide an earlier mount — or the implicit repo
///    bind — is refused (see [`check_hidden_mounts`]); and a dest
///    BELOW a writable bind is refused, because bubblewrap follows
///    symlinks in a dest's parent components and writable content can
///    plant them (see [`check_symlinkable_dests`], review-2 item 2).
/// 6. environment via `--setenv`, in this precedence: host-forwarded
///    variables first, then `cfg.env` (which wins by being set later),
///    then the infrastructure variables `HOME` and `PATH` last — set
///    after `cfg.env` on purpose, so neither layer can point them
///    somewhere else (config.md D14)
/// 7. `--chdir` into the repo root
/// 8. `--` and the payload, verbatim
///
/// Deliberately absent (see the base table's "no" rows): `/run`, `~/tmp`,
/// a host-backed `/tmp/<name>`, the host home directory (only the empty
/// tmpfs [`SANDBOX_HOME`] serves as `$HOME`), and any automatic
/// `OPENAI_API_KEY` —
/// under `mysbx` a key is an ordinary user-config `[env]` entry
/// (docs/design/config.md D6). Nothing is forwarded implicitly: only the
/// variables the caller put in `host_env` reach the sandbox.
pub fn bwrap_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
    host_env: &HostEnv,
    params: &Params<'_>,
) -> Result<Vec<String>, Error> {
    let root = repo.root.to_string_lossy().into_owned();
    let mut argv: Vec<String> = vec!["--clearenv".into(), "--unshare-all".into()];
    if cfg.network {
        argv.push("--share-net".into());
        // Sharing the network namespace alone does not give the new
        // root DNS or TLS: `/etc/resolv.conf` and friends live on the
        // host and are not part of the base table. Bind the resolver
        // set — the same path SET the `network` combinator of
        // `fns/bubblewrap-app.nix` binds (its mechanism differs:
        // runtime-deep-ro-bind walks entries and re-binds symlink
        // targets; a plain `--ro-bind-try` is enough here because
        // bwrap resolves a symlinked source at mount time, and on
        // NixOS `/etc/ssl` resolves through `/etc/static` into
        // `/nix/store`, which is a base bind — `/etc/static` and
        // `/etc/ca-certificates` of the simpler wrapper serve other
        // distros' layouts). `--ro-bind-try`: every entry is
        // setup-dependent — a static `/etc/resolv.conf` needs only the
        // file, systemd-resolved symlinks it into
        // `/run/systemd/resolve` (bound as a directory, mirroring the
        // reference), `/etc/nsswitch.conf` may be unnecessary when
        // glibc defaults suffice; a dangling symlink silently drops
        // that one bind, like the reference's try-readonly.
        // `network = false` shares nothing and binds none of them
        // (review-1 finding 5).
        for path in RESOLVER_PATHS {
            argv.push("--ro-bind-try".into());
            argv.push((*path).into());
            argv.push((*path).into());
        }
        // The nix daemon socket and store database live under
        // `/nix/var/nix`. They are bound ONLY here, with the network
        // (review-2 item 3): a read-only bind does not stop the
        // payload from connecting to the socket, and the daemon
        // happily builds fixed-output derivations, which are exactly
        // the ones that keep network access. Exposing it under
        // `network = false` would make the report's "denied" a lie.
        // The price is that `nix` needs the shared network to work at
        // all — said out loud in plan.md's base table.
        argv.push("--ro-bind-try".into());
        argv.push("/nix/var/nix".into());
        argv.push("/nix/var/nix".into());
    }

    // 3. the base binds (docs/plan.md "The base" table, fixed absolute
    // host paths — machine-independent), plus the sanitized nix client
    // configuration when the wrapper pinned one (review-2 item 3 — the
    // host's own nix.conf stays out, it may hold access-tokens).
    argv.extend(base_binds());
    if let Some(nix_conf) = params.nix_conf {
        // `--ro-bind`, not `-try`: the pin is a store path the wrapper
        // just built, so a missing one is a packaging bug that must
        // fail loudly rather than silently drop the configuration.
        argv.push("--ro-bind".into());
        argv.push(nix_conf.into());
        argv.push("/etc/nix/nix.conf".into());
    }

    // 4. the repo, rw, at its real host path (D13), plus the git
    // metadata directories a `.git` FILE points at outside the root
    // (linked worktrees, submodules — review-1 finding 4): git needs
    // them rw to update refs and the index. Common dir first so a
    // gitdir nested inside it stays reachable in the degenerate
    // layout (a later equal-or-ancestor bind would hide it).
    // Review-2 item 1: the pointer lives in a repo-writable file, so
    // it is NOT a mount specification — every target must be at or
    // below an entry of `cfg.git_dirs`, the approval list of the
    // trusted layers, before it is bound. `/`, the home directory and
    // anything related to a protected sandbox path are never
    // approvable and are refused outright.
    bind(&mut argv, false, &root, None);
    for git_dir in &repo.git_dirs {
        check_git_dir(git_dir, &cfg.git_dirs)?;
        bind(&mut argv, false, &git_dir.to_string_lossy(), None);
    }

    // 4b. the `state-dirs` binds (docs/design/config.md D15): one rw
    // bind per declared entry — the host source synthesized from the
    // sidecar (`<sidecar>/state/<entry>`, created by the CLI before
    // the backend starts, since bwrap needs an existing source), the
    // dest below the sandbox home (`/mysbx-home/<entry>`), where the
    // tmpfs of section 3 lands it on the first run and the payload's
    // writes persist in the sidecar across runs. They are implicit
    // infrastructure like the repo bind, not configuration: no
    // `[[mounts]]` entry may cover their dests (the hidden-mount
    // check below treats them like the repo and the git dirs), and
    // no entry may nest inside another — see [`check_state_dirs`].
    let state_binds: Vec<(String, String)> = cfg
        .state_dirs
        .iter()
        .map(|entry| {
            (
                repo.sidecar
                    .join("state")
                    .join(entry)
                    .to_string_lossy()
                    .into_owned(),
                format!("{SANDBOX_HOME}/{entry}"),
            )
        })
        .collect();
    check_state_dirs(&cfg.state_dirs)?;
    for (_src, dest) in &state_binds {
        // Defense in depth: the parser already rejects every spelling
        // that could leave the sandbox home, and a state dest is a
        // strict descendant of [`SANDBOX_HOME`], so this can only
        // fire if the constant itself ever moves onto a protected
        // path — fail loudly then, not at mount time.
        if let Some(protected) = check_dest(dest) {
            return Err(Error::ProtectedDest {
                dest: dest.clone(),
                protected,
            });
        }
    }
    for (src, dest) in &state_binds {
        bind(&mut argv, false, src, Some(dest));
    }

    // Review-3 item 3: a writable bind may never expose a trusted
    // policy file — the user config (host-wide grants) or the sidecar
    // config (this repo's own sandbox policy). The payload writing one
    // steers the NEXT run: a `git-dirs` approval can be added, the
    // `.git` pointer rewritten to match. `rw` mounts are the direct
    // case; the repo bind and the git dirs are `rw` too, so they are
    // checked as well — a sidecar or user config sitting inside the
    // work tree is refused, not silently exposed. `ro` mounts do not
    // count: the payload cannot write through them.
    for src in cfg
        .mounts
        .iter()
        .filter(|m| m.mode == Mode::Rw)
        .map(|m| normalize(&m.path))
        .chain(std::iter::once(normalize(&root)))
        .chain(repo.git_dirs.iter().map(|g| normalize(&g.to_string_lossy())))
    {
        for policy in params.policy_paths {
            let pol = normalize(&policy.to_string_lossy());
            if pol.starts_with(&src) {
                return Err(Error::PolicyFileWritable {
                    source: src.to_string_lossy().into_owned(),
                    policy: policy.display().to_string(),
                });
            }
        }
        // The same argument for the state tree (config.md D15): the
        // sidecar's `state/` directory is not a policy FILE, but its
        // layout decides the SOURCES of the next run's state binds. A
        // writable bind of an ANCESTOR of a backing store lets the
        // payload swap a component for a symlink, so the next run
        // would bind whatever it points at rw into the sandbox home.
        // `ensure_state_dirs` refuses to follow such a symlink, but
        // that fails a later run with a filesystem diagnosis; the
        // configuration that made it possible is refused here, where
        // the operator can still read it as a config error. The
        // backing store ITSELF stays mountable (the payload already
        // has it rw, and it cannot rewrite its own parent), and so
        // does any `ro` view of the tree.
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

    // 5. configured mounts, in declaration order; dest defaults to the
    // canonicalized source path. A `dest` may never be related to a
    // protected path in either direction: bubblewrap applies binds in
    // order with later-mounts-win, so a dest of `/tmp`, `/`, `/proc` …
    // would overwrite a base bind, and a dest of `/nix` would hide the
    // protected `/nix/store` below it — either way reopening exactly
    // the hole the base table closes. And no mount may HIDE an earlier
    // one: a later bind whose dest is a strict ancestor of an earlier
    // mount's dest replaces that subtree wholesale, so the earlier
    // entry would be dead configuration (see [`check_hidden_mounts`]).
    // The merge (D7/D8) resolves the paths of both layers; these
    // validate the argv layout, because the base list and the order
    // semantics live here. They run for EVERY mount, from either
    // layer: neither file may bind onto a protected sandbox path.
    // The protected-dest check runs FIRST: a dest that overwrites a base
    // bind is the sharper diagnosis, and the repo-covering check would
    // otherwise mask it with a generic `would hide` for dests like `/`.
    // (The policy-file check above is even earlier; a config violating
    // several rules reports the policy exposure, which is the one that
    // turns the NEXT run into a widened one.)
    for m in &cfg.mounts {
        let dest = m.dest.as_deref().unwrap_or(&m.path);
        if let Some(protected) = check_dest(dest) {
            // A mount that does not redirect (dest == source) hits
            // this only when a config declares a protected path as its
            // own source (`path = "/proc"`); the usual hit is a
            // redirect onto a protected path.
            return Err(Error::ProtectedDest {
                dest: dest.to_string(),
                protected,
            });
        }
    }
    if !cfg.network {
        // The daemon is bound with `--share-net` and nowhere else
        // (section 2) — but a configured mount could still source it.
        // Its dest is irrelevant: what matters is that the socket
        // becomes reachable at all. Both containment directions count
        // (review-3 item 2): a source AT OR BELOW `/nix/var/nix` is one,
        // and so is a HOST ANCESTOR — binding `/nix` or `/` read-only
        // still exposes `/nix/var/nix/daemon-socket/socket` through
        // the wider window. `check_dest` and `check_hidden_mounts`
        // already refuse the in-sandbox ancestors of protected
        // paths, so an ancestor SOURCE was the one gap.
        const DAEMON_DIR: &str = "/nix/var/nix";
        // Every effective source, not just the configured mounts:
        // the implicit repo bind is checked too (review-3 item 2 said
        // so explicitly). In practice a repo cannot sit there — `/`
        // and the home tree are refused at discovery — but `/nix` or
        // `/nix/var` are ordinary directories, and the rule is cheap.
        for src in cfg
            .mounts
            .iter()
            .map(|m| normalize(&m.path))
            .chain(std::iter::once(normalize(&root)))
        {
            if src.starts_with(DAEMON_DIR) || Path::new(DAEMON_DIR).starts_with(&src) {
                return Err(Error::DaemonUnderDeniedNetwork {
                    source: src.to_string_lossy().into_owned(),
                });
            }
        }
    }
    check_hidden_mounts(&cfg.mounts, &root, &repo.git_dirs, &state_binds)?;
    check_symlinkable_dests(&cfg.mounts, &root, &repo.git_dirs, &state_binds)?;
    for m in &cfg.mounts {
        bind(&mut argv, m.mode == Mode::Ro, &m.path, m.dest.as_deref());
    }

    // 6. environment: host-forwarded first, then `[env]` (later
    // `--setenv` wins), then the dev-tool `PATH` last.
    for (key, value) in host_env {
        argv.push("--setenv".into());
        argv.push(key.clone());
        argv.push(value.clone());
    }
    for (key, value) in &cfg.env {
        argv.push("--setenv".into());
        argv.push(key.clone());
        argv.push(value.clone());
    }
    // `HOME` and `PATH` are infrastructure, not configuration: they name
    // paths this builder created (the tmpfs of section 3, the tool
    // closure of `params`), so a layer that could repoint them would
    // break the sandbox rather than configure it (config.md D14). Set
    // last: the later `--setenv` wins, so `[env]` cannot override them.
    argv.push("--setenv".into());
    argv.push("HOME".into());
    argv.push(SANDBOX_HOME.into());
    argv.push("--setenv".into());
    argv.push("PATH".into());
    argv.push(params.tools_path.into());

    // 7. work in the repo.
    argv.push("--chdir".into());
    argv.push(root.clone());

    // 8. the payload, verbatim.
    argv.push("--".into());
    match payload {
        Payload::Shell => argv.push(params.shell.into()),
        Payload::Command(args) => argv.extend(args.iter().cloned()),
    }

    Ok(argv)
}

/// Why the argv cannot be built safely (review-2 item 4): a
/// user-reachable configuration that cannot be laid out is an ordinary
/// error — the CLI reports it on stderr with its `mysbx: ` prefix and
/// exits `1` (cli.md D8/D9), never a Rust panic. Both variants keep the
/// exact message texts the panic era asserted, so the diagnosis a user
/// sees did not change with the representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// The mount's `dest` would shadow or overwrite a protected sandbox
    /// path (base table of docs/plan.md).
    ProtectedDest {
        /// The offending destination.
        dest: String,
        /// The protected path it is related to.
        protected: &'static str,
    },
    /// A later mount's dest hides an earlier bind — an earlier mount, or
    /// an implicit one (the repo root, a git metadata directory).
    HiddenMount { message: String },
    /// A `.git` FILE points at git metadata outside the repo that no
    /// trusted layer approved (review-2 item 1): the bind is refused,
    /// because a repo-writable pointer must not become a mount
    /// specification.
    GitDirNotApproved { gitdir: PathBuf },
    /// A mount `dest` lies below a writable bind — the repo, a git
    /// metadata directory, an `rw` mount, or a `ro` alias of any of
    /// those. bubblewrap resolves the destination path in the sandbox
    /// it has built so far and FOLLOWS symlinks in its parent
    /// components, so a symlink planted in that writable content
    /// redirects the bind to any path at all (review-2 item 2,
    /// review-3 item 1).
    DestBelowWritable {
        /// The refused destination.
        dest: String,
        /// The writable bind it lies below.
        writable: String,
    },
    /// A writable bind (the repo, a git dir, or an `rw` mount) would
    /// expose a trusted policy file — the user config or the sidecar
    /// config — to the payload (review-3 item 3). A policy file the
    /// sandbox can write makes the NEXT run a widened one: `git-dirs`
    /// approvals can be added, the `.git` pointer rewritten to match.
    PolicyFileWritable {
        /// The mount (or repo) source the policy file lies below.
        source: String,
        /// The policy file that would become writable.
        policy: String,
    },
    /// A writable bind (the repo, a git dir, or an `rw` mount) would
    /// expose an ANCESTOR of a `state-dirs` backing store to the
    /// payload (docs/design/config.md D15). The state tree decides
    /// where the next run's state binds come from: with a writable
    /// parent the payload can replace a component with a symlink, and
    /// the next run would bind its target rw into the sandbox home.
    /// `lib.rs::ensure_state_dirs` refuses to follow such a symlink;
    /// this refuses the configuration that allows planting it.
    StateTreeWritable {
        /// The mount (or repo) source the backing store lies below.
        source: String,
        /// The state backing store that would become replaceable.
        state_dir: String,
    },
    /// A configured mount would carry the nix daemon into a sandbox
    /// whose network is denied (review-2 item 3, review-3 item 2). The
    /// socket under `/nix/var/nix` is a network service: the daemon
    /// builds fixed-output derivations, which keep network access, so
    /// a mount that sources it — or any HOST ANCESTOR of it, like
    /// `/nix` itself, which carries the socket along — would make
    /// `network = false` a lie no matter what its dest is.
    DaemonUnderDeniedNetwork { source: String },
    /// The git metadata a `.git` FILE points at is related to a
    /// protected sandbox path — the bind would shadow or overwrite base
    /// infrastructure exactly like a bad mount dest, so no approval can
    /// make it safe (review-2 item 1).
    GitDirProtected {
        gitdir: PathBuf,
        protected: &'static str,
    },
    /// Two `state-dirs` entries nest (docs/design/config.md D15): one
    /// is a strict ancestor of the other. The later bind would land
    /// on the sidecar subtree of the earlier one (or vice versa,
    /// depending on argv order) and silently redirect the narrower
    /// entry's backing directory — the layout is ambiguous, so the
    /// configuration is refused instead.
    StateDirNesting {
        /// The entry declared first (the ancestor).
        outer: String,
        /// The entry declared later (the descendant) that would nest.
        inner: String,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ProtectedDest { dest, protected } => write!(
                f,
                "mount dest {dest} would shadow or overwrite the protected \
                 sandbox path {protected}, which mysbx reserves whether or \
                 not this run binds it (base table of docs/plan.md); \
                 refusing to build the argv"
            ),
            Error::HiddenMount { message } => f.write_str(message),
            Error::GitDirNotApproved { gitdir } => write!(
                f,
                "git metadata {} is not approved — a repo-writable .git \
                 file must not become a mount specification \
                 (review-2 item 1); approve the directory in \
                 `git-dirs` in the user config or sidecar \
                 (docs/design/config.md D8), or drop the pointer",
                gitdir.display()
            ),
            Error::DestBelowWritable { dest, writable } => write!(
                f,
                "mount dest {dest} lies below {writable}, whose content the \
                 sandbox can write — bubblewrap resolves a dest through the \
                 sandbox it has built so far and follows symlinks in its \
                 parent components, so a symlink planted there redirects this \
                 bind onto any path, protected ones included; mount it \
                 outside that tree instead"
            ),
            Error::PolicyFileWritable { source, policy } => write!(
                f,
                "source {source} would expose the policy file {policy} \
                 writable — a config the sandbox can write steers the NEXT \
                 run of itself (git-dirs approvals, .git pointers); narrow \
                 the mount to below it, or drop it"
            ),
            Error::StateTreeWritable { source, state_dir } => write!(
                f,
                "source {source} would expose the state directory {state_dir} \
                 to the payload through a writable PARENT — the sandbox could \
                 then replace it with a symlink and redirect the next run's \
                 state bind out of the sidecar (docs/design/config.md D15); \
                 mount it read-only, or narrow the mount to the state \
                 directory itself"
            ),
            Error::DaemonUnderDeniedNetwork { source } => write!(
                f,
                "source {source} is inside or above the nix daemon directory \
                 /nix/var/nix, and this sandbox denies the network — the daemon \
                 builds fixed-output derivations, which keep network access, so a \
                 bind exposing the socket would hand back exactly what \
                 `network = false` takes away; drop the bind or share the network"
            ),
            Error::GitDirProtected { gitdir, protected } => write!(
                f,
                "git metadata {} would shadow or overwrite the protected \
                 sandbox path {protected} (base table of docs/plan.md); \
                 no approval can make that safe \u{2014} move the repository \
                 out of {protected}",
                gitdir.display()
            ),
            Error::StateDirNesting { outer, inner } => write!(
                f,
                "state-dirs entries nest: `{inner}` is below `{outer}` — \
                 each entry gets its own bind of \
                 <sidecar>/state/<entry> at /mysbx-home/<entry>, and a \
                 nested one would land inside the other's backing directory; \
                 declare only the narrowest entry (docs/design/config.md D15)"
            ),
        }
    }
}

impl std::error::Error for Error {}

/// The resolver and TLS paths bound read-only when the network is
/// shared (review-1 finding 5) — the same path SET as the `network`
/// combinator of `fns/bubblewrap-app.nix` (see the call site for why
/// the mechanism can be a plain `--ro-bind-try` here).
static RESOLVER_PATHS: &[&str] = &[
    "/etc/hosts",
    "/etc/nsswitch.conf",
    "/etc/resolv.conf",
    "/etc/ssl",
    "/run/systemd/resolve",
];

/// The fixed base binds of the MVP (docs/plan.md, base table). Every row
/// with decision "yes" appears exactly once, in the order the existing
/// `fns/bubblewrap-app.nix` base binds them (agents shell out to
/// arbitrary store paths → `/nix/store` first; `/usr/bin/env` shebangs
/// → `/usr/bin`; timezones → `/etc/localtime`; a fresh tmpfs `/tmp`,
/// NOT the host-backed one).
///
/// The two nix binds review-1 finding 6 added are NOT here (review-2
/// item 3): `/nix/var/nix` carries the daemon socket and rides with
/// the network switch instead (section 2), and the host
/// `/etc/nix/nix.conf` is never bound at all — a sanitized
/// replacement comes from [`Params::nix_conf`].
fn base_binds() -> Vec<String> {
    vec![
        "--ro-bind".into(),
        "/nix/store".into(),
        "/nix/store".into(),
        "--ro-bind".into(),
        "/usr/bin".into(),
        "/usr/bin".into(),
        "--proc".into(),
        "/proc".into(),
        "--dev".into(),
        "/dev".into(),
        "--ro-bind".into(),
        "/etc/localtime".into(),
        "/etc/localtime".into(),
        "--tmpfs".into(),
        "/tmp".into(),
        // `$HOME` inside the sandbox: an empty, writable tmpfs, so
        // `cd ~`, `~/.bash_history` and every tool that insists on a
        // home directory work — without the host home being reachable
        // (config.md D14).
        "--tmpfs".into(),
        SANDBOX_HOME.into(),
    ]
}

/// Sandbox paths a mount `dest` may never shadow or overwrite — the
/// roots the base binds create (`/nix/store`, `/nix/var/nix`,
/// `/etc/nix/nix.conf`, `/usr/bin`, `/proc`, `/dev`, `/etc/localtime`,
/// `/tmp`) plus `/run` (no wholesale `/run`
/// bind exists — the only `/run` path mounted is the resolver
/// exception [`RESOLVER_PATHS`], ro and narrow — so dests related to
/// `/run` as a whole are still refused) — and `/` itself, which would
/// shadow every one of them at once. A dest is refused when it is
/// RELATED to any of these in either direction: equal, a descendant
/// (`/proc/sys` would overwrite part of the procfs), or an ANCESTOR
/// (`/nix` would receive the mount and hide `/nix/store` below it) —
/// see [`check_dest`]. The repo root
/// is deliberately NOT here: it is a base bind of its own (section 4)
/// and a mount legitimately points at or below it.
/// [`SANDBOX_HOME`] is NOT here either, for the same reason: seeding the
/// sandbox home with host dotfiles (`~/.gitconfig`, an agent config) by
/// pointing a mount `dest` into it is the intended way to use it, and
/// such a mount is an explicit entry of one of the two trusted config
/// layers (config.md D6/D7).
/// The tmpfs is created in section 3, so those mounts land on top of it.
/// What IS refused for [`SANDBOX_HOME`] is a dest equal to or above it
/// (review-2 item 5): that would replace the tmpfs itself rather than
/// seed it, leaving `HOME` pointing at content no layer declared —
/// see the one-directional check at the top of [`check_dest`].
/// The resolver paths are likewise not protected: an explicit mount
/// with dest `/etc/ssl` (say, to install a project-local CA) shadows the
/// ro-bind-try by later-wins — intended, same reasoning as
/// [`SANDBOX_HOME`]; a dest of `/etc/resolv.conf` does NOT reach
/// `/etc/localtime` or `/run` and so is not refused either.
static PROTECTED_DESTS: &[&str] = &[
    "/",
    "/nix/store",
    "/nix/var/nix",
    "/etc/nix/nix.conf",
    "/usr/bin",
    "/proc",
    "/dev",
    "/etc/localtime",
    "/tmp",
    "/run",
];

/// The protected path a mount `dest` would shadow or overwrite, if
/// any. The dest is normalized lexically first (see [`normalize`]); the
/// merge (`crate::merge`) guarantees that source PATHS are
/// canonicalized against the host, but a `dest` deliberately never is
/// (it is an in-sandbox path) — so normalization is this function's
/// job. Symlinks are NOT resolved here: they would need
/// host-filesystem knowledge of the sandbox's new root, which does not
/// exist at argv-build time.
fn check_dest(dest: &str) -> Option<&'static str> {
    let path = normalize(dest);
    for protected in PROTECTED_DESTS {
        let protected_path = Path::new(protected);
        // `/` must match EXACTLY on the normalized path: every absolute
        // dest is at-or-below `/` by construction, so a prefix match
        // there would refuse every legitimate dest. The `..`-cases
        // (`/x/..`) are already collapsed onto `/` by `normalize`, so
        // the exact match catches them. Every other protected path
        // matches in BOTH directions on whole components
        // (`Path::starts_with`): a dest at-or-below it (a descendant
        // such as `/proc/sys` would replace part of the procfs the base
        // bind provides) and a dest that contains it (an ancestor such
        // as `/nix` would receive the mount and hide the protected
        // `/nix/store` below it). Lookalikes stay allowed: `/usr/bin2`
        // is NOT `/usr/bin`.
        let hits = if *protected == "/" {
            path == *Path::new("/")
        } else {
            path.starts_with(protected_path) || protected_path.starts_with(&path)
        };
        if hits {
            return Some(protected);
        }
    }
    // [`SANDBOX_HOME`] is protected in ONE direction only (review-2
    // item 5): a dest equal to it — or an ancestor of it — replaces
    // or hides the tmpfs the base binds created, while the report
    // still says `HOME=/mysbx-home` and the payload gets a home nobody
    // declared. Strict DESCENDANTS stay allowed: seeding dotfiles into
    // the home by pointing a `dest` there is the documented way to use
    // it (config.md D14). On component boundaries the sandbox home's
    // only ancestor is `/`, which the list above already refuses — so
    // this check runs AFTER it and effectively guards the EQUAL case,
    // keeping the sharper "would shadow `/`" answer for the root.
    // `/mysbx` is a string prefix, not an ancestor: a different
    // directory, and it stays mountable like `/usr/bin2`.
    if Path::new(SANDBOX_HOME).starts_with(&path) {
        return Some(SANDBOX_HOME);
    }
    None
}

/// A later bind whose dest is a strict ancestor of an earlier mount's
/// dest hides that earlier mount entirely: bubblewrap applies binds
/// in argv order with later-wins per subtree, so the wide bind simply
/// replaces the subtree the narrow one landed on. That silently undoes
/// restrictions — `/home/u/.ssh` (ro) followed by `/home/u` (rw) leaves
/// `.ssh` writable — and silently kills mounts the other way round
/// (`/home/u` rw followed by `/home/u/.ssh` ro does not HIDE anything —
/// though review-2 item 2 refuses it one guard later, because the
/// narrow dest resolves through writable content, see
/// [`check_symlinkable_dests`]). Equal dests do not hide: re-binding
/// the same subtree narrows by shadowing, and since both config layers
/// are trusted (config.md D7) the last bind on an equal dest simply
/// wins — within a layer and across the two alike. The implicit
/// binds — the repo root (always rw, D13) and the git metadata
/// directories a `.git` FILE points at (review-1 finding 4) — count as
/// entries BEFORE every configured mount, and an EQUAL dest is
/// refused there too: implicit binds are not configuration, and a
/// mount that replaces the repo (even rw) changes what `--chdir` lands
/// in; one that covers a git dir breaks `git` inside the sandbox.
/// Returns [`Error::HiddenMount`] on violation: this guards the argv
/// layout, and a config that cannot be laid out safely must not run —
/// as an ordinary CLI error (review-2 item 4), never a panic.
/// Docs/design/config.md D15: `state-dirs` entries may not nest. Each
/// entry is bound rw at `/mysbx-home/<entry>` with its backing store at
/// `<sidecar>/state/<entry>`, so a nested pair (`.local/share` and
/// `.local/share/opencode`) would bind one backing directory inside the
/// other's subtree: the later bind lands on `<sidecar>/state/.local/share/
/// opencode` — a path that only exists as the earlier entry's own backing
/// store — and the narrower entry's writes would silently go to a
/// DIFFERENT physical directory than a flat declaration would use. The
/// configuration is ambiguous, so it is refused rather than second-guessed:
/// declare only the narrowest entries you need. Checked pairwise in
/// declaration order; the first offender is reported (outer = the
/// ancestor, inner = the descendant, whichever was declared first).
fn check_state_dirs(state_dirs: &[String]) -> Result<(), Error> {
    for (i, outer) in state_dirs.iter().enumerate() {
        for inner in &state_dirs[i + 1..] {
            let a = format!("/{outer}/");
            let b = format!("/{inner}/");
            if a.starts_with(&b) || b.starts_with(&a) {
                return Err(Error::StateDirNesting {
                    outer: outer.clone(),
                    inner: inner.clone(),
                });
            }
        }
    }
    Ok(())
}

fn check_hidden_mounts(
    mounts: &[Mount],
    repo_root: &str,
    git_dirs: &[PathBuf],
    state_binds: &[(String, String)],
) -> Result<(), Error> {
    // Implicit binds come before every configured mount: the repo root
    // and the git metadata directories a `.git` file points at. A
    // configured mount whose dest covers any of them replaces that
    // subtree wholesale. The implicit set is discovered per run, so it
    // cannot be anticipated in configuration: covering it is refused in
    // EVERY form, equal dest included, because the mount would not just
    // shadow an entry — it would replace implicit infrastructure.
    let mut implicit: Vec<(PathBuf, &str)> =
        vec![(normalize(repo_root), "the repo working tree")];
    for g in git_dirs {
        implicit.push((normalize(&g.to_string_lossy()), "a git metadata directory"));
    }
    for (_src, dest) in state_binds {
        implicit.push((normalize(dest), "a state directory"));
    }
    for (later_i, later) in mounts.iter().enumerate() {
        let later_dest = normalize(later.dest.as_deref().unwrap_or(&later.path));
        // The implicit binds come before every configured mount; a dest
        // at-or-below them (equal included) covers them.
        for (implicit_dest, what) in &implicit {
            if implicit_dest.starts_with(&later_dest) {
                return Err(Error::HiddenMount {
                    message: format!(
                        "mount {} ({}) would hide {} — implicit binds are not configuration and always come first, so a dest may not cover them",
                        later_dest.display(),
                        later.path,
                        what,
                    ),
                });
            }
        }
        for earlier in &mounts[..later_i] {
            let earlier_dest =
                normalize(earlier.dest.as_deref().unwrap_or(&earlier.path));
            if later_dest == earlier_dest {
                continue; // equal dests: shadowing re-bind, not hiding
            }
            if earlier_dest.starts_with(&later_dest) {
                return Err(Error::HiddenMount {
                    message: format!(
                        "mount {} ({}) would hide earlier mount {} ({}) — bubblewrap applies binds in order, so a wider dest must come FIRST; swap the entries or drop one",
                        later_dest.display(),
                        later.path,
                        earlier_dest.display(),
                        earlier.path,
                    ),
                });
            }
        }
    }
    Ok(())
}

/// Review-2 item 2: a `dest` is an in-sandbox path, and bubblewrap
/// resolves it against the sandbox it has built SO FAR — following
/// symlinks in the parent components. Everything the guards above can
/// check is lexical: `check_dest` normalizes `.` and `..`, but it
/// cannot know that `<repo>/jump` is a symlink to `/`, which turns a
/// dest of `<repo>/jump/tmp` into the protected `/tmp`.
///
/// Host-side `canonicalize()` is no fix: it models the HOST tree, not
/// the composed sandbox root, and it races with the payload that may
/// rewrite the tree between the check and the bind. The MVP therefore
/// refuses the whole class instead: a dest may not lie BELOW a bind
/// whose content is writable, because that content is exactly where
/// such a symlink can be planted —
///
/// - the repo (always rw, D13) and the git metadata directories: the
///   payload writes them, and what it writes persists to the next run,
/// - any `rw` mount: same argument, one layer out.
///
/// An `ro` bind whose SOURCE is ordinary host state is not in the set:
/// the sandbox cannot change that content, so the residual risk is a
/// symlink the user themselves put in their own declared directory —
/// the accident barrier, not the malice barrier (D9). An `ro` bind
/// that re-exposes writable content IS in the set, though: `ro` stops
/// writes THROUGH the bind, not writes to the same host inode through
/// the repo bind next door, so a `ro` mount of `<repo>/tools` is as
/// symlink-plantable as the repo itself.
///
/// The writable base binds need no entry: `/tmp` and every path below
/// it are already refused by [`check_dest`], and [`SANDBOX_HOME`] is a
/// tmpfs bubblewrap creates empty in this very run — nothing can have
/// planted a symlink there, which is what keeps dotfile seeding
/// (config.md D14) possible.
/// An EQUAL dest is not below anything and stays allowed: re-binding
/// the same path resolves the path itself, not a component inside the
/// writable content.
///
/// The analysis is order-INDEPENDENT (review-3 item 1) although
/// bubblewrap applies binds in order: the symlink a payload plants
/// persists to the NEXT run, and on that next run the declaration
/// order is identical — a guard that depended on the order would
/// only defend the first run against a pattern whose exploit is the
/// second. So the writable sets are built from the mount list as a
/// whole, in a fixed-point pass: every `rw` mount contributes its
/// source to `writable_sources` and its dest to `writable_dests`,
/// every `ro` mount whose source is (or comes to be) inside a
/// writable source contributes its dest too, and contributions are
/// propagated in BOTH directions until nothing changes.
fn check_symlinkable_dests(
    mounts: &[Mount],
    repo_root: &str,
    git_dirs: &[PathBuf],
    state_binds: &[(String, String)],
) -> Result<(), Error> {
    // HOST paths whose content the sandbox can write. The repo (rw by
    // D13) and the git metadata directories start the set; an `rw`
    // mount adds its source, because the payload writes the host path
    // through it.
    let mut writable_sources: Vec<PathBuf> = vec![normalize(repo_root)];
    writable_sources.extend(git_dirs.iter().map(|g| normalize(&g.to_string_lossy())));
    // IN-SANDBOX paths below which a dest may not land, because their
    // content is one of the writable sources above. The repo and the
    // git dirs are bound at their host path, so they are both.
    // State dirs are rw binds too — the payload persists its agent
    // state there — so their sources join the writable set and their
    // dests the in-sandbox set. The SEEDING carve-out of D14 is not
    // weakened by them: the tmpfs home itself stays seedable (its
    // direct dests are refused one-directionally), only content BELOW
    // a state dir becomes symlink-plantable, exactly like repo
    // subdir content.
    for (src, _dest) in state_binds {
        let src = normalize(src);
        if !writable_sources.contains(&src) {
            writable_sources.push(src);
        }
    }
    let mut writable_dests: Vec<PathBuf> = writable_sources.clone();
    for (_src, dest) in state_binds {
        let dest = normalize(dest);
        if !writable_dests.contains(&dest) {
            writable_dests.push(dest);
        }
    }

    // Order-independence (review-3 item 1): passes run until a
    // fixed point, so it does not matter which alias is declared
    // first — an `ro` parent that contains writable content
    // anywhere below it contributes its dest, and a mount's own
    // dest may not be below any writable dest, whenever declared.
    loop {
        let mut changed = false;
        for m in mounts {
            let dest = normalize(m.dest.as_deref().unwrap_or(&m.path));
            let src = normalize(&m.path);
            if let Some(prefix) = writable_dests
                .iter()
                .find(|p| dest.starts_with(p) && dest != **p)
            {
                return Err(Error::DestBelowWritable {
                    dest: dest.to_string_lossy().into_owned(),
                    writable: prefix.to_string_lossy().into_owned(),
                });
            }
            // A mount makes its dest subtree writable-in-sandbox when
            // it is `rw` — and also when it is `ro` but re-exposes
            // content that is writable elsewhere in the sandbox, in
            // EITHER direction: `ro` stops the payload from writing
            // THROUGH this bind, not from writing the same host inode
            // through the repo bind next door. A source INSIDE a
            // writable subtree is the review-2 case; a source
            // CONTAINING one (an `ro` alias of a tree holding the
            // repo, or of a parent of an `rw` mount's source) exposes
            // the same planted symlinks through the wider window
            // (review-3 item 1).
            let src_is_writable = writable_sources
                .iter()
                .any(|w| src.starts_with(w) || w.starts_with(&src));
            let push_if_new = |set: &mut Vec<PathBuf>, p: PathBuf| {
                if !set.contains(&p) {
                    set.push(p);
                    true
                } else {
                    false
                }
            };
            if m.mode == Mode::Rw {
                changed |= push_if_new(&mut writable_sources, src);
                changed |= push_if_new(&mut writable_dests, dest);
            } else if src_is_writable {
                changed |= push_if_new(&mut writable_dests, dest);
            }
        }
        if !changed {
            break;
        }
    }
    Ok(())
}

/// Review-2 item 1: refuse to bind git metadata a repo-writable `.git`
/// FILE points at unless a trusted layer (user config or sidecar)
/// approved the directory. The approval list `approved` holds
/// canonicalized host paths (D8); the target `gitdir` is canonicalized
/// too (repo.rs), so the containment is symlink-resolved on both
/// sides. Refusals:
///
/// - a target related to a protected sandbox path — `/` itself, an
///   ancestor of `/nix/store`, anything at or below `/tmp` …: the
///   bind would shadow or overwrite base infrastructure exactly like
///   a configured mount dest, so no approval can make it safe
///   ([`Error::GitDirProtected`]). The home directory is refused one
///   step earlier, at repo resolution (`crate::repo`), which is the
///   layer that knows `$HOME`.
/// - a target that is at-or-below NO approved entry: the pointer is
///   untrusted content (config.md D3) and grants nothing.
fn check_git_dir(gitdir: &Path, approved: &[PathBuf]) -> Result<(), Error> {
    // Protected paths (`/` among them): the bind lands at the git dir's real host path,
    // so a gitdir related to one is as bad as a mount dest that is.
    // `check_dest` expects the normalized in-sandbox spelling; host
    // paths are already absolute and `..`-free after canonicalize,
    // but normalize anyway so the comparison matches the dest rules.
    let dest = normalize(&gitdir.to_string_lossy());
    if let Some(protected) = check_dest(&dest.to_string_lossy()) {
        return Err(Error::GitDirProtected {
            gitdir: gitdir.to_owned(),
            protected,
        });
    }
    let ok = approved
        .iter()
        .any(|a| dest.starts_with(normalize(&a.to_string_lossy())));
    if !ok {
        return Err(Error::GitDirNotApproved {
            gitdir: gitdir.to_owned(),
        });
    }
    Ok(())
}

/// Lexically resolve `.` and `..` components of an absolute path —
/// mirroring how the kernel (and therefore bubblewrap, which mounts at
/// the path it is given after resolving it) interprets a destination
/// written with redundant components. A dest is plain string data from
/// the TOML config, so checking it untrusted here is what makes the
/// protected-dest guard tamper-proof (`"/nix/../proc"` must be seen as
/// `/proc`). `/..` stays `/` — the kernel resolves the root's parent as
/// itself.
fn normalize(p: &str) -> PathBuf {
    let mut out: Vec<std::ffi::OsString> = Vec::new();
    for c in Path::new(p).components() {
        match c {
            Component::RootDir => out.clear(),
            Component::CurDir => {}
            Component::ParentDir => {
                // `..` above the root stays at the root (`out` never
                // holds the root component itself — it is re-attached at
                // the end — so popping to empty is climbing to `/`).
                if !out.is_empty() {
                    out.pop();
                }
            }
            other => out.push(other.as_os_str().to_owned()),
        }
    }
    if out.is_empty() {
        PathBuf::from("/")
    } else {
        out.into_iter().fold(PathBuf::from("/"), |mut acc, c| {
            acc.push(c);
            acc
        })
    }
}

/// One bind of a host path into the sandbox, ro or rw, with the sandbox
/// destination defaulting to the source path.
fn bind(argv: &mut Vec<String>, ro: bool, src: &str, dest: Option<&str>) {
    argv.push(if ro {
        "--ro-bind".into()
    } else {
        "--bind".into()
    });
    argv.push(src.into());
    argv.push(dest.unwrap_or(src).into());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Mount;
    use std::path::PathBuf;

    /// A synthetic repo root. [`bwrap_argv`] is pure: it canonicalizes
    /// nothing and checks no existence, so the synthetic paths need not
    /// exist — and need not be stable across runs.
    fn synth_repo() -> Repo {
        Repo {
            root: PathBuf::from("/synth/repo"),
            sidecar: PathBuf::from("/synth/repo.mysbx"),
            git_dirs: Vec::new(),
        }
    }

    fn merged() -> Merged {
        Merged {
            backend: Some("bubblewrap".into()),
            network: true,
            mounts: Vec::new(),
            env: BTreeMap::new(),
            git_dirs: Vec::new(),
            state_dirs: Vec::new(),
        }
    }

    fn params() -> Params<'static> {
        Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            nix_conf: None,
            policy_paths: &[],
        }
    }

    fn shell_repo_defaults() -> (Repo, Merged, Params<'static>) {
        (synth_repo(), merged(), params())
    }

    /// Position of the first argument equal to `needle`.
    fn pos(argv: &[String], needle: &str) -> usize {
        argv.iter()
            .position(|x| x == needle)
            .unwrap_or_else(|| panic!("missing {needle}"))
    }

    #[test]
    fn sections_in_order() {
        // Byte-for-byte coverage lives in tests/argv.rs; this asserts the
        // SPEC ORDER of the sections (spec "Watch out": a refactor must
        // not reorder sections 3-5).
        let (repo, cfg, p) = shell_repo_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        assert_eq!(argv[0], "--clearenv");
        assert_eq!(argv[1], "--unshare-all");
        assert_eq!(argv[2], "--share-net");
        assert!(pos(&argv, "/nix/store") < pos(&argv, "/usr/bin"));
        assert!(pos(&argv, "--proc") < pos(&argv, "--dev"));
        assert!(pos(&argv, "--dev") < pos(&argv, "/etc/localtime"));
        assert!(pos(&argv, "--tmpfs") < pos(&argv, "--bind"));
        assert!(pos(&argv, "--chdir") < pos(&argv, "--"));
    }

    #[test]
    fn shell_payload_after_dashdash() {
        let (repo, cfg, p) = shell_repo_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        let n = argv.len();
        assert!(n >= 2);
        assert_eq!(argv[n - 2], "--");
        assert_eq!(argv[n - 1], "/synth/bin/bash");
    }

    #[test]
    fn command_payload_is_verbatim() {
        let (repo, cfg, p) = shell_repo_defaults();
        let payload = Payload::Command(vec!["ls".into(), "-x".into(), "--help".into()]);
        let argv = bwrap_argv(&cfg, &repo, &payload, &HostEnv::new(), &p)
        .unwrap();
        let n = argv.len();
        assert_eq!(&argv[n - 4..], &["--", "ls", "-x", "--help"]);
        // Flag-looking arguments stay verbatim payload content (cli.md D4).
        assert!(argv.contains(&"--help".to_string()));
        // The only `--` separator is the one before the payload.
        assert_eq!(argv.iter().filter(|x| x.as_str() == "--").count(), 1);
    }

    #[test]
    fn mounts_in_declaration_order_with_dest_default() {
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![
            Mount {
                path: "/synth/ro-src".into(),
                dest: None,
                mode: Mode::Ro,
            },
            Mount {
                path: "/synth/nested".into(),
                dest: Some("/inside/dest".into()),
                mode: Mode::Rw,
            },
        ];
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        // A mount without `dest` binds at its own source path; the rw
        // mount with an explicit dest uses it verbatim. The ro bind must
        // precede the rw bind (mount order is argv order).
        let bind_pairs: Vec<(&String, &String)> = argv
            .windows(3)
            .filter(|w| w[0] == "--ro-bind" || w[0] == "--bind")
            .map(|w| (&w[1], &w[2]))
            .collect();
        // The base binds come first (fixed paths), then the repo, then
        // the mounts — and the mounts keep declaration order, with dest
        // defaulting to the source path.
        assert_eq!(
            bind_pairs
                .iter()
                .map(|(src, dst)| (src.as_str(), dst.as_str()))
                .collect::<Vec<_>>(),
            vec![
                ("/nix/store", "/nix/store"),
                ("/usr/bin", "/usr/bin"),
                ("/etc/localtime", "/etc/localtime"),
                ("/synth/repo", "/synth/repo"),
                ("/synth/ro-src", "/synth/ro-src"),
                ("/synth/nested", "/inside/dest"),
            ],
            "declaration order and dest defaulting"
        );
    }

    #[test]
    fn network_false_denies() {
        let (repo, cfg, p) = shell_repo_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        assert!(argv.contains(&"--share-net".to_string()));

        let mut deny = cfg;
        deny.network = false;
        let argv = bwrap_argv(&deny, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        assert!(!argv.contains(&"--share-net".to_string()));
        // But --unshare-all stays.
        assert!(argv.contains(&"--unshare-all".to_string()));
    }

    #[test]
    fn env_precedence_host_then_config_then_path() {
        let mut cfg = merged();
        cfg.env.insert("TERM".into(), "cfg-wins".into());
        cfg.env.insert("CFG_ONLY".into(), "1".into());
        let mut host = HostEnv::new();
        host.insert("TERM".into(), "host-val".into());
        host.insert("EDITOR".into(), "host-nvim".into());
        let repo = synth_repo();
        let p = params();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &host, &p)
        .unwrap();
        // Every `--setenv` triple, in argv order.
        let setenvs: Vec<usize> = argv
            .iter()
            .enumerate()
            .filter(|(_, x)| x.as_str() == "--setenv")
            .map(|(i, _)| i)
            .collect();
        // Host-forwarded first (keys in BTreeMap order) …
        assert_eq!(
            &argv[setenvs[0]..setenvs[0] + 3],
            &["--setenv", "EDITOR", "host-nvim"]
        );
        assert_eq!(
            &argv[setenvs[1]..setenvs[1] + 3],
            &["--setenv", "TERM", "host-val"]
        );
        // … then [env]: the later `TERM` wins by being set later; keys in
        // BTreeMap order, so `CFG_ONLY` sorts before `TERM`.
        assert_eq!(
            &argv[setenvs[2]..setenvs[2] + 3],
            &["--setenv", "CFG_ONLY", "1"]
        );
        assert_eq!(
            &argv[setenvs[3]..setenvs[3] + 3],
            &["--setenv", "TERM", "cfg-wins"]
        );
        // … then the infrastructure variables, `HOME` and `PATH` last of
        // all env; nothing follows them but the `--chdir` and payload
        // sections.
        assert_eq!(
            &argv[setenvs[4]..setenvs[4] + 3],
            &["--setenv", "HOME", SANDBOX_HOME]
        );
        assert_eq!(argv[setenvs[5]], "--setenv");
        assert_eq!(argv[setenvs[5] + 1], "PATH");
        assert_eq!(argv[setenvs[5] + 2], "/synth/bin");
        assert_eq!(
            argv.len() - setenvs[5] - 3,
            4, // --chdir /synth/repo -- /synth/bin/bash
            "nothing after PATH but --chdir, -- and the payload"
        );
    }

    #[test]
    fn no_run_no_host_home_no_openai() {
        let (repo, cfg, p) = shell_repo_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        let joined = argv.join(" ");
        // No WHOLESALE `/run` bind (the base table's `no` row: D-Bus,
        // the nix-daemon socket, agent sockets). The resolver exception
        // of review-1 finding 5 is narrow and ro: exactly
        // `/run/systemd/resolve`, only when the network is shared.
        assert!(
            !argv.windows(3).any(|w| w[0] == "--ro-bind" && w[1] == "/run"),
            "no wholesale /run bind"
        );
        assert!(
            !argv.windows(3).any(|w| w[0] == "--bind" && w[1] == "/run"),
            "no wholesale /run bind (rw)"
        );
        // No host home BIND, no host home path, no `~/tmp` — and no
        // automatic secret forwards (the OPENAI row of the base table).
        // `$HOME` inside the sandbox is the tmpfs of the base table's
        // `$HOME` row (config.md D14), which is a different claim.
        assert!(!joined.contains("$HOME"));
        assert!(!joined.contains("/home/"), "no host home path anywhere");
        assert!(!joined.contains("OPENAI_API_KEY"));
        assert!(!argv.contains(&"~/tmp".to_string()));
        // Every bind source is a base path or the repo — the sandbox home
        // is a tmpfs, i.e. backed by nothing on the host.
        let sources: Vec<&str> = argv
            .windows(3)
            .filter(|w| w[0] == "--ro-bind" || w[0] == "--bind")
            .map(|w| w[1].as_str())
            .collect();
        assert!(!sources.iter().any(|s| s.contains("home")), "{sources:?}");
    }

    #[test]
    fn sandbox_home_is_a_tmpfs_and_is_exported_as_home() {
        // config.md D14: `$HOME` exists inside the sandbox (so `cd ~`
        // works), is an empty tmpfs, and is not below `/home`.
        let (repo, cfg, p) = shell_repo_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        let tmpfs: Vec<&str> = argv
            .windows(2)
            .filter(|w| w[0] == "--tmpfs")
            .map(|w| w[1].as_str())
            .collect();
        assert_eq!(tmpfs, vec!["/tmp", SANDBOX_HOME]);
        let i = pos(&argv, "HOME");
        assert_eq!(&argv[i - 1..i + 2], &["--setenv", "HOME", SANDBOX_HOME]);
        assert!(!SANDBOX_HOME.starts_with("/home"));
        // The tmpfs is created before the configured mounts, so a mount
        // may seed the home; the `--setenv HOME` comes after them.
        assert!(pos(&argv, SANDBOX_HOME) < pos(&argv, "--setenv"));
    }

    #[test]
    fn config_env_cannot_override_home() {
        // Same rule as `PATH` (config.md D14): `HOME` is infrastructure,
        // set after `[env]`, so the later `--setenv` wins.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.env.insert("HOME".into(), "/synth/evil-home".into());
        let mut host = HostEnv::new();
        host.insert("HOME".into(), "/synth/host-home".into());
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &host, &p)
        .unwrap();
        let last = argv
            .iter()
            .enumerate()
            .filter(|(_, x)| x.as_str() == "HOME")
            .map(|(i, _)| i)
            .next_back()
            .unwrap();
        assert_eq!(
            &argv[last - 1..last + 2],
            &["--setenv", "HOME", SANDBOX_HOME]
        );
        // The rejected values are still in the argv (the builder does not
        // filter them), but the effective value is the last one.
        assert!(argv.contains(&"/synth/evil-home".to_string()));
    }

    #[test]
    fn mount_dest_onto_tmp_is_refused() {
        // The hole the base table explicitly closes: binding a host path
        // ONTO /tmp reconstructs the host-backed /tmp. Later mounts win
        // in bubblewrap, so this must never build an argv.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/host-tmp".into(),
            dest: Some("/tmp".into()),
            mode: Mode::Rw,
        }];
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::ProtectedDest { protected: "/tmp", .. }),
            "wrong error: {err}"
        );
    }

    #[test]
    fn mount_dest_onto_root_is_refused() {
        // A dest of / would shadow /proc, /dev and everything else in one
        // move.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/everything".into(),
            dest: Some("/".into()),
            mode: Mode::Rw,
        }];
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::ProtectedDest { .. }),
            "wrong error: {err}"
        );
    }

    #[test]
    fn mount_dest_below_proc_is_refused() {
        // Nested, not just exact: a dest under /proc would overwrite part
        // of the procfs the base bind provides.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/proc-faker".into(),
            dest: Some("/proc/sys".into()),
            mode: Mode::Ro,
        }];
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::ProtectedDest { .. }),
            "wrong error: {err}"
        );
    }

    #[test]
    fn mount_dest_onto_ancestor_of_protected_path_is_refused() {
        // The ancestor hole: a dest of `/nix` would receive the mount and
        // hide the protected `/nix/store` below it; likewise `/usr` hides
        // `/usr/bin` and `/etc` hides `/etc/localtime`.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/data".into(),
            dest: Some("/nix".into()),
            mode: Mode::Rw,
        }];
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::ProtectedDest { .. }),
            "wrong error: {err}"
        );
    }

    #[test]
    fn mount_dest_etc_ancestor_is_refused() {
        // Second ancestor case, pinned separately so a refactor cannot fix
        // `/nix` while leaving `/etc` (hiding `/etc/localtime`) open.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/data".into(),
            dest: Some("/etc".into()),
            mode: Mode::Ro,
        }];
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::ProtectedDest { .. }),
            "wrong error: {err}"
        );
    }

    #[test]
    fn mount_dest_dotdot_to_root_is_refused() {
        // The lexical hole: `/x/..` passes a plain string check but
        // bubblewrap resolves it to `/`, mounting over everything.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/everything".into(),
            dest: Some("/x/..".into()),
            mode: Mode::Rw,
        }];
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::ProtectedDest { .. }),
            "wrong error: {err}"
        );
    }

    #[test]
    fn mount_dest_dotdot_into_protected_is_refused() {
        // Same, aimed at a narrower protected path: `/nix/../proc` is
        // `/proc` after lexical resolution.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/proc-faker".into(),
            dest: Some("/nix/../proc".into()),
            mode: Mode::Rw,
        }];
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::ProtectedDest { .. }),
            "wrong error: {err}"
        );
    }

    #[test]
    fn mount_dest_redundant_component_spellings_are_refused() {
        // Every spelling that resolves onto a protected path: `..`
        // overshoot from the root (`/../../proc` is `/proc`), a `.` run
        // (`/proc/./sys`), a trailing `.` (`/tmp/.`), and duplicate
        // slashes (`//tmp///x`). The kernel drops or collapses all of
        // them, so the guard must normalize before it compares.
        for (i, dest) in [
            "/../../proc",
            "/proc/./sys",
            "/tmp/./.",
            "//tmp///x",
            "/nix/store/../../..",
        ]
        .into_iter()
        .enumerate()
        {
            let (repo, cfg, p) = shell_repo_defaults();
            let mut cfg = cfg;
            cfg.mounts = vec![Mount {
                path: "/synth/data".into(),
                dest: Some(dest.into()),
                mode: Mode::Ro,
            }];
            let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
            assert!(
                matches!(err, Err(Error::ProtectedDest { .. })),
                "dest {i} ({dest}) must be refused, got: {err:?}"
            );
        }
    }

    #[test]
    fn dotdot_out_of_protected_stays_allowed() {
        // `/tmp/../synth/dest` is `/synth/dest` — a `..` used to climb OUT
        // of a protected path is ordinary and must stay mountable.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/data".into(),
            dest: Some("/tmp/../synth/dest".into()),
            mode: Mode::Ro,
        }];
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        let pairs: Vec<_> = argv
            .windows(3)
            .filter(|w| w[0] == "--ro-bind")
            .map(|w| (w[1].as_str(), w[2].as_str()))
            .collect();
        assert!(pairs.contains(&("/synth/data", "/tmp/../synth/dest")));
    }

    #[test]
    fn component_similar_dests_stay_allowed() {
        // The guard is component-exact: string-prefixed look-alikes are
        // distinct paths and must stay mountable.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![
            Mount {
                path: "/synth/a".into(),
                dest: Some("/usr/bin2".into()),
                mode: Mode::Ro,
            },
            Mount {
                path: "/synth/b".into(),
                dest: Some("/tmpx".into()),
                mode: Mode::Ro,
            },
            Mount {
                path: "/synth/c".into(),
                dest: Some("/nix/storex".into()),
                mode: Mode::Ro,
            },
        ];
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
        .unwrap();
        let cfg_dests: Vec<&str> = argv
            .windows(3)
            .filter(|w| w[0] == "--ro-bind" || w[0] == "--bind")
            .map(|w| w[2].as_str())
            // The repo bind (a --bind of the repo itself, section 4) and
            // the base binds are not this test's subject.
            .filter(|d| !d.starts_with("/synth/repo") && *d != "/nix/store" && *d != "/usr/bin" && *d != "/etc/localtime")
            .collect();
        assert_eq!(cfg_dests, vec!["/usr/bin2", "/tmpx", "/nix/storex"]);
    }

    #[test]
    fn mount_dest_inside_repo_is_refused() {
        // Review-2 item 2 turned this around: a dest inside the repo
        // used to be the "legitimate remap", but the repo is writable
        // and bubblewrap follows symlinks in a dest's parent
        // components — `<repo>/jump -> /` makes `<repo>/jump/tmp`
        // land on the protected `/tmp`. The whole class is refused;
        // mount outside the work tree instead.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![Mount {
            path: "/synth/data".into(),
            dest: Some("/synth/repo/.data".into()),
            mode: Mode::Ro,
        }];
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::DestBelowWritable { .. }),
            "wrong error: {err}"
        );
    }
}
