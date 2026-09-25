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

use crate::config::{Mode, Mount, Multiplexer};
use crate::merge::Merged;
use crate::repo::Repo;
use std::collections::BTreeMap;
use std::fmt;
use std::path::{Component, Path, PathBuf};

/// Which tree a run works in — the two modes of the workspace model
/// (docs/design/workspace.md D1). A CLI-layer fact, never a config
/// one: there is no `workspace` key in either layer (D1 — an unknown
/// key is a schema error, config.md D11), and a session name is a
/// per-run fact, not repository policy.
///
/// - [`Live`] is the default and byte-identical to today's argv: the
///   repo itself is the workspace, the implicit always-rw bind of
///   config.md D13.
/// - [`Clone`] is selected by `--session NAME`: the session's clone
///   at `<repo>.mysbx/clones/NAME` is bound rw AT THE REPO'S OWN
///   PATH (D3 — path identity is preserved), while the host repo,
///   the `__worktrees` sibling, the git metadata directories and the
///   `state-dirs` binds are NOT mounted at all, and every configured
///   mount is downgraded to read-only (D4) — the clone is the only
///   writable bind of a clone run.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Workspace<'a> {
    /// The default: the repo itself, always rw (config.md D13).
    #[default]
    Live,
    /// `--session NAME`: the clone at `clone`, bound rw at the repo's
    /// own path. The name itself is carried by
    /// [`crate::session::Session`]; the argv builder needs only the
    /// path.
    Clone {
        /// The session's clone, `<repo>.mysbx/clones/NAME`.
        clone: &'a Path,
    },
}

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
/// itself; item 5 collects the forwarded host variables
/// (`FORWARDED_ENV_VARS`, lib.rs — the terminal/locale block plus the
/// model-credential block of bd myconfig-20j, each only when set) into
/// this map.
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

/// The directory holding the private socket of a multiplexer payload
/// (docs/design/config.md D16, generalized by D17), exported as
/// `TMUX_TMPDIR` and used by the entry scripts as
/// `tmux -S $TMUX_TMPDIR/socket`.
///
/// It is a path **inside** the sandbox home tmpfs and nothing else:
/// no host path is bound at or below it, and no configuration may
/// make one land there (see [`check_mux_socket`]). The socket is
/// therefore reachable only from this one sandbox — never from
/// another mysbx sandbox of the same repository, never from a tmux
/// server on the host, and it dies with the tmpfs. That is the whole
/// isolation claim of the multiplexer integration, and it rests on the
/// path being infrastructure: `TMUX_TMPDIR` is emitted after `[env]`,
/// like `HOME` and `PATH`, so no layer can repoint it at
/// `/tmp/tmux-1000` or at a bound host directory.
///
/// The value is set for EVERY session-starting choice, including the
/// one that is not a tmux server (`herdr`, which keeps its own socket
/// and state under the tmpfs `HOME`): one code path, and a
/// pane that runs plain `tmux` inside such a session lands on this
/// private socket instead of the host default `/tmp/tmux-<uid>`.
pub const MUX_SOCKET_DIR: &str = "/mysbx-home/.mysbx-tmux";

/// The `WAYLAND_DISPLAY` name the waypipe server presents its fake
/// compositor socket under (docs/design/config.md D18). A bare name,
/// deliberately: the multi-mode server creates it in
/// `$XDG_RUNTIME_DIR`, which the argv pins at [`SANDBOX_HOME`], so the
/// socket lands at `/mysbx-home/wayland-0` — a path inside the sandbox
/// home tmpfs by construction.
pub const WAYPIPE_DISPLAY: &str = "wayland-0";

/// The in-sandbox path of the waypipe server's display socket:
/// `$XDG_RUNTIME_DIR/<WAYPIPE_DISPLAY>` with `XDG_RUNTIME_DIR` pinned
/// at [`SANDBOX_HOME`] (D18). No bind ever lands at or below it (see
/// [`check_display_socket`]); the socket itself is created by the
/// multi-mode guest server inside the sandbox, not mounted.
pub const WAYPIPE_DISPLAY_PATH: &str = "/mysbx-home/wayland-0";

/// Common parameters of every invocation that do not come from a
/// configuration layer: the shell binary and the dev-tool `PATH` closure
/// root, both host paths the MVP carries in its own closure
/// (docs/plan.md: "Payload shell", "dev-tool closure on `PATH`").
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Params<'a> {
    /// Path of the shell used for [`Payload::Shell`].
    pub shell: &'a str,
    /// The dev-tool closure's `bin` directory, set as `PATH` inside the
    /// sandbox (git, tig, ripgrep, fd, jq, nix, python3, coreutils, …).
    pub tools_path: &'a str,
    /// A shell to bind read-only at `/bin/sh` inside the sandbox, or
    /// `None` for no `/bin/sh` at all.
    ///
    /// The sandbox root is not a FHS root: `/bin` exists nowhere in
    /// the base table (docs/plan.md "The base"), only `/nix/store` and
    /// `/usr/bin` (itself holding just `env` on plain NixOS). But
    /// `/bin/sh` is a de-facto ABI of the Unix userland — tmux runs
    /// EVERY `run-shell`/`if-shell`/`#()` job through `execl("/bin/sh",
    /// …)` (tmux ≥ 3.5a reverted to hardcoding `_PATH_BSHELL` for jobs;
    /// `default-shell` applies to panes and popups only), `posix_spawn`
    /// of several tools falls back to it, and a plain `#!/bin/sh`
    /// shebang needs it. Without the bind every such job dies with
    /// `execl failed` before the payload command even starts — on the
    /// workmux sidebar this surfaced as `'kill -USR1 $(tmux show-option
    /// …)' returned 1` popups and sidebars that never appear. The Nix
    /// wrapper pins `bash`'s own `bin/sh` symlink here (the same bash
    /// closure [`Params::shell`] comes from — the vendored
    /// `vendor/alexdavid-jail.nix` base combinator binds exactly this);
    /// an unwrapped build passes `None` and the sandbox runs without a
    /// `/bin/sh`, like it runs without a pinned nix.conf.
    pub bin_sh: Option<&'a str>,
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
    /// The **CA bundle** pinned from mysbx's own closure (`nss-cacert`'s
    /// `ca-bundle.crt`), or `None` when this build pinned none (bd
    /// myconfig-938).
    ///
    /// The resolver-set bind of `/etc/ssl` + `/etc/static` makes TLS work
    /// on a NixOS host with its standard layout, but the argv promises
    /// more than "works here": a host whose `/etc` layout differs, whose
    /// ca-bundle is stale, or a payload tool that looks no further than
    /// `SSL_CERT_FILE` should not be the reason a sandboxed agent cannot
    /// reach its model endpoint. The gvisor agent-image tier pins the
    /// same bundle for the same reason (agent-image.nix sets
    /// `SSL_CERT_FILE`/`GIT_SSL_CAINFO`/`NIX_SSL_CERT_FILE` in the image
    /// env). This pin is that mechanism's bubblewrap equivalent: the
    /// wrapper pins a bundle from its OWN closure — reproducible, no
    /// host state — and the argv sets the three env variables to it
    /// AFTER `[env]`, like `HOME` and `PATH`, because they are
    /// infrastructure for the same reason: a layer that repointed them
    /// at a host path would widen the sandbox's view of the host `/etc`,
    /// not configure the run. (An unwrapped build passes `None` and the
    /// run relies on the resolver binds alone.)
    pub ca_bundle: Option<&'a str>,
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
    /// file turns the NEXT run into a widened one — a `git-dirs`
    /// approval can be added by the attacker, and the `.git` pointer
    /// rewritten to match (review-3 item 3). `rw` mount sources that
    /// contain one are therefore refused; the check also covers the
    /// repo bind itself, which is `rw` by definition.
    pub policy_paths: &'a [PolicyPath],
    /// The **multiplexer entry** the interactive payload is replaced
    /// by when the merged configuration selects one
    /// (docs/design/config.md D17, cli.md D11) — the entry of THAT
    /// multiplexer, resolved by the caller from
    /// [`Multiplexer::entry_var`], or `None` when this build pinned
    /// none for it.
    ///
    /// Like [`Params::shell`] this is a host path from mysbx's own
    /// closure, pinned by the Nix wrapper (`MYSBX_MUX_ENTRY_*`); the
    /// script it names starts the multiplexer on the in-sandbox state
    /// of [`MUX_SOCKET_DIR`] and attaches to it. There is no fallback
    /// on purpose: a selected multiplexer with nothing pinned is a
    /// refused run ([`Error::MultiplexerUnavailable`]), never a silent
    /// plain shell — the operator asked for a session, and getting a
    /// bare shell instead would be discovered only after the work was
    /// done in the wrong place.
    pub mux_entry: Option<&'a str>,
    /// The **waypipe channel** of a run whose merged `display` is
    /// `waypipe` (docs/design/config.md D18): the host directory
    /// holding the per-run `waypipe.sock` the host-side `waypipe
    /// client` creates and the argv binds rw into the sandbox, plus
    /// the guest `waypipe server` binary that wraps the payload.
    /// `None` when this build pinned no waypipe (`MYSBX_WAYPIPE`) —
    /// a selected display is then a refused run
    /// ([`Error::DisplayUnavailable`]), never a silently headless one.
    ///
    /// Both values are infrastructure like [`Params::shell`]: the
    /// socket directory is created by the CLI per run (under the
    /// sidecar, outside the sandbox's reach) and holds exactly one
    /// waypipe socket; the guest binary comes from mysbx's own
    /// closure, pinned by the Nix wrapper. The socket FILE is always
    /// `<socket_dir>/waypipe.sock` — the same path on both ends of the
    /// channel, bound rw at itself so the guest finds it where the
    /// host created it.
    pub waypipe: Option<Waypipe<'a>>,
    /// Which tree this run works in (workspace.md D1): the live repo
    /// (the default, unchanged) or a named session's clone (D3/D4 —
    /// the argv differences are exactly the workspace's). A
    /// CLI-layer fact, like every other field of `Params`: it does
    /// not come from a configuration layer, and no TOML key can name
    /// it (workspace.md D1).
    pub workspace: Workspace<'a>,
}

/// The waypipe channel of a `display = "waypipe"` run
/// (docs/design/config.md D18): the per-run host socket directory the
/// CLI created (the host-side `waypipe client` listens on
/// `<socket_dir>/waypipe.sock`) and the guest `waypipe server` binary
/// from mysbx's own closure that wraps the payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Waypipe<'a> {
    /// Host directory holding the per-run socket file `waypipe.sock`.
    /// Created symlink-free by the CLI under the sidecar
    /// (`<sidecar>/waypipe/<pid>`), bound rw at itself, never created by
    /// a `--dry-run`.
    pub socket_dir: &'a str,
    /// The `waypipe` binary INSIDE the sandbox's reach (a store path
    /// from mysbx's own closure, on the sandbox `PATH` and pinned by
    /// the wrapper as `MYSBX_WAYPIPE`) — the SERVER end that presents
    /// the fake compositor socket to the payload and connects to the
    /// client's socket.
    pub guest_bin: &'a str,
}

/// A trusted policy file, represented by everything the payload must
/// not be able to write for the file to still be the policy on the
/// NEXT run (review-4 item 1).
///
/// Protecting only the fully resolved target is not enough. mysbx does
/// not find its policy by inode — it walks a PATHNAME, and every
/// directory entry on that walk decides which file the next run reads.
/// Home Manager writes `~/.config/mysbx/config.toml` as a SYMLINK into
/// the Nix store: the resolved target lives in the immutable
/// `/nix/store`, but the symlink itself sits in an ordinary,
/// replaceable directory. A writable bind covering that directory lets
/// the payload unlink the symlink and drop a policy of its own in its
/// place — the next run then reads the attacker's file while the
/// target-only check saw nothing but `/nix/store/…`.
///
/// [`guarded`](Self::guarded) therefore holds BOTH: every directory
/// entry traversed to reach the file (with the parents of each entry
/// already resolved, so intermediate symlinks are listed as the
/// entries they are, not only as what they point at) AND the final
/// resolved target. A writable source that contains any of them is
/// refused.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PolicyPath {
    /// The pathname mysbx used, for the diagnosis (the spelling the
    /// operator recognises).
    pub path: PathBuf,
    /// Host paths that must stay unwritable: the traversed directory
    /// entries — including intermediate and final symlinks — and the
    /// resolved target. Built by `crate::trusted_policy`, which walks
    /// the pathname on the host filesystem; the argv builder itself
    /// stays pure and only compares.
    pub guarded: Vec<PathBuf>,
}

impl PolicyPath {
    /// A policy path guarded by its pathname alone — no host
    /// filesystem is consulted. For synthetic paths (tests) and as the
    /// fallback when the walk cannot resolve anything: the pathname is
    /// always part of the guarded set, so this is a narrowing of the
    /// protection, never a widening.
    pub fn lexical(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        Self {
            guarded: vec![path.clone()],
            path,
        }
    }
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
///    tmpfs `/dev/shm`, `/etc/localtime` ro, tmpfs `/tmp`,
///    tmpfs [`SANDBOX_HOME`]
/// 4. the repo itself, read-write, at its real host path
///    (docs/design/config.md D13), followed by the git metadata
///    directories its `.git` FILE points at, also rw (review-1 finding 4:
///    worktrees and submodules are unusable without them), then the
///    `state-dirs` binds (config.md D15): each declared entry backed
///    by `<sidecar>/state/<entry>` and bound rw at `/mysbx-home/<entry>`,
///    then — for a `display = "waypipe"` run — the per-run waypipe
///    socket directory, bound rw at itself (config.md D18)
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
///    somewhere else (config.md D14) — plus the CA-bundle variables
///    `SSL_CERT_FILE`/`GIT_SSL_CAINFO`/`NIX_SSL_CERT_FILE` when the
///    wrapper pinned a bundle (infrastructure for the same reason,
///    bd myconfig-938), plus `TMUX_TMPDIR`
///    ([`MUX_SOCKET_DIR`]) for a run with a multiplexer, which is
///    infrastructure for the same reason (config.md D16/D17), and
///    `XDG_RUNTIME_DIR` (the tmpfs home) for a run with the waypipe
///    display, likewise infrastructure (config.md D18)
/// 7. `--chdir` into the repo root
/// 8. `--` and the payload, verbatim — except that the *interactive*
///    payload of a run with `multiplexer = …` is that multiplexer's
///    pinned entry instead of the shell (config.md D17, cli.md D11);
///    `run -- CMD` is untouched — and that a run with
///    `display = "waypipe"` is wrapped in the guest waypipe server
///    (config.md D18), which presents the fake compositor socket to
///    the payload and connects to the host client's socket
///
/// Deliberately absent (see the base table's "no" rows): `/run`, `~/tmp`,
/// a host-backed `/tmp/<name>`, and the host home directory (only the
/// empty tmpfs [`SANDBOX_HOME`] serves as `$HOME`). The builder itself
/// still forwards NOTHING implicitly — only the variables the caller put
/// in `host_env` reach the sandbox. The CLI's collection step defaults to
/// technical variables only (terminal, locale, editor); a credential
/// reaches the sandbox solely when a layer names it in `forward-env`.
/// See `FORWARDED_ENV_VARS` (lib.rs) and docs/plan.md "Environment".
pub fn bwrap_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
    host_env: &HostEnv,
    params: &Params<'_>,
) -> Result<Vec<String>, Error> {
    let root = repo.root.to_string_lossy().into_owned();
    // The state entries a run actually binds (docs/design/config.md
    // D22): the declared `state-dirs` plus the implicit `.ssh` of a
    // run with `ssh-key` — computed once, before the guards, so the
    // socket checks and the bind emission below see one list.
    let effective_state_dirs = cfg.effective_state_dirs();
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
        check_mux_socket(&cfg.mounts, &effective_state_dirs)?;
        if params.mux_entry.is_none() {
            return Err(Error::MultiplexerUnavailable { multiplexer: mux });
        }
    }
    // The display channel applies to EVERY payload form (D18): unlike
    // the multiplexer it is not a session, and a `run -- CMD` whose
    // command opens a window needs it just as much as the interactive
    // shell. The guards and the wrap below therefore see the merged
    // value directly.
    if cfg.display.is_waypipe() {
        check_display_socket(&cfg.mounts, &effective_state_dirs)?;
        if params.waypipe.is_none() {
            return Err(Error::DisplayUnavailable);
        }
    }
    let mut argv: Vec<String> = vec!["--clearenv".into(), "--unshare-all".into()];
    if cfg.network {
        argv.push("--share-net".into());
        // Sharing the network namespace alone does not give the new
        // root DNS or TLS: `/etc/resolv.conf` and friends live on the
        // host and are not part of the base table. Bind the resolver
        // set — the same path SET the `network` combinator of
        // `fns/bubblewrap-app.nix` binds, plus `/etc/static`, which
        // the simpler wrapper `fns/bubblewrap-simple-app.nix` taught
        // us about: bwrap DOES resolve a symlinked SOURCE at mount
        // time, but it does NOT follow the symlinks INSIDE the bound
        // tree at lookup time — on NixOS `/etc/ssl/certs/ca-bundle.crt`
        // and `/etc/ssl/trust-source` are themselves symlinks pointing
        // at `/etc/static/ssl/...`, and for a long time the comment
        // here claimed bwrap "resolves the chain into `/nix/store`,
        // which is a base bind". It does not (bd myconfig-938,
        // observed live: the targets dangled and every TLS tool
        // inside the sandbox failed with `unable to get local issuer
        // certificate`). `/etc/static` is therefore bound like its
        // siblings, `--ro-bind-try`, so the symlink farm resolves;
        // the simpler wrapper walks exactly the same set with
        // runtime-deep-ro-bind, which re-binds symlink targets
        // individually — a plain `--ro-bind-try` of the whole tree is
        // enough here because bwrap mounts the directory itself, and
        // every entry under it then follows in the sandbox like on
        // the host). `--ro-bind-try`: every entry is
        // setup-dependent — a static `/etc/resolv.conf` needs only the
        // file, systemd-resolved symlinks it into
        // `/run/systemd/resolve` (bound as a directory, mirroring the
        // reference), `/etc/nsswitch.conf` may be unnecessary when
        // glibc defaults suffice, and `/etc/static` is a NixOS-ism
        // other distros do not carry; a dangling symlink silently
        // drops that one bind, like the reference's try-readonly.
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
    // `/bin/sh` for the sandbox (see [`Params::bin_sh`] for why the
    // minimal root must grow one). Bound with the section's own
    // base-bind idiom, `--ro-bind`, not `-try`: the pin is a store
    // path from mysbx's own closure, so a missing one is a packaging
    // bug that must fail loudly — the same reasoning as the nix.conf
    // pin below. The dest is a base-bind root and therefore protected
    // ([`PROTECTED_DESTS`]): no configured mount may shadow the shell
    // every job-spawning tool in the sandbox agrees on.
    if let Some(bin_sh) = params.bin_sh {
        argv.push("--ro-bind".into());
        argv.push(bin_sh.into());
        argv.push("/bin/sh".into());
    }
    if let Some(nix_conf) = params.nix_conf {
        // `--ro-bind`, not `-try`: the pin is a store path the wrapper
        // just built, so a missing one is a packaging bug that must
        // fail loudly rather than silently drop the configuration.
        argv.push("--ro-bind".into());
        argv.push(nix_conf.into());
        argv.push("/etc/nix/nix.conf".into());
    }

    // 4. the workspace bind — the repo, rw, at its real host path
    // (D13), plus the git metadata directories a `.git` FILE points
    // at outside the root (linked worktrees, submodules — review-1
    // finding 4): git needs them rw to update refs and the index.
    // Common dir first so a gitdir nested inside it stays reachable in
    // the degenerate layout (a later equal-or-ancestor bind would
    // hide it). Review-2 item 1: the pointer lives in a
    // repo-writable file, so it is NOT a mount specification —
    // every target must be at or below an entry of `cfg.git_dirs`,
    // the approval list of the trusted layers, before it is bound.
    // `/`, the home directory and anything related to a protected
    // sandbox path are never approvable and are refused outright.
    //
    // In a CLONE run (workspace.md D3) the section shrinks to ONE
    // bind: the session's clone, rw, AT THE REPO'S OWN PATH — path
    // identity is preserved, tools keyed to the repo path work
    // unchanged. The host repo is NOT mounted at all, the
    // `__worktrees` sibling is NOT bound (it is operator state of
    // the live checkout), and the git-dirs approvals do not apply
    // either: the clone carries its own `.git` DIRECTORY, not a
    // pointer, so there is nothing external to approve.
    let (workspace_src, workspace_dest, implicit_rw_sources): (String, String, Vec<PathBuf>) =
        match params.workspace {
            Workspace::Live => {
                bind(&mut argv, false, &root, None);
                for git_dir in &repo.git_dirs {
                    check_git_dir(git_dir, &cfg.git_dirs)?;
                    bind(&mut argv, false, &git_dir.to_string_lossy(), None);
                }
                // 4a. the workmux worktrees sibling, rw, at its real host
                // path — implicit infrastructure like the repo bind
                // (D13), discovered per run and therefore inexpressible
                // in configuration, exactly like the git metadata above.
                // Bound ONLY when it exists (see [`Repo::worktrees`]): a
                // run never creates it, so existence is the operator's
                // trust decision; an absent sibling keeps the sandbox
                // narrow, and `workmux add` inside it fails with a
                // filesystem error naming the path — the honest outcome
                // for a directory no layer declared.
                if let Some(worktrees) = &repo.worktrees {
                    bind(&mut argv, false, &worktrees.to_string_lossy(), None);
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
                bind(&mut argv, false, &clone.to_string_lossy(), Some(&root));
                // The clone is the ONLY writable bind of a clone run
                // (D4) — the one HOST source the writable-set analyses
                // start from, and the one rw source the policy-file
                // refusal checks. It sits in the sidecar, but the bind
                // is exactly the clone directory: it covers neither
                // `config.toml` nor `state/` (D3, "policy-file
                // adjacency"). The in-sandbox path of the writable tree
                // stays the REPO's own path (D3: path identity is
                // preserved) — source and dest differ in a clone run,
                // which the live run's conflation of the two never
                // showed.
                (
                    clone.to_string_lossy().into_owned(),
                    root.clone(),
                    vec![normalize(&clone.to_string_lossy())],
                )
            }
        };
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
    // A run with `ssh-key` (docs/design/config.md D22) binds the same
    // shape for its implicit `.ssh` entry: the effective list carries
    // it, so the generated keypair lands at `/mysbx-home/.ssh` where
    // ssh and git look for it.
    //
    // In a CLONE run the `state-dirs` are NOT handled at all
    // (workspace.md D4): no backing store is created, nothing is
    // bound — per-session versus shared agent state is deliberately
    // deferred to a follow-up bead, and scratch space stays the
    // tmpfs home and `/tmp`, as in any run.
    let state_binds: Vec<(String, String)> = if let Workspace::Clone { .. } = params.workspace {
        Vec::new()
    } else {
        check_state_dirs(&effective_state_dirs)?;
        effective_state_dirs
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
            .collect()
    };
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

    // 4c. the waypipe socket bind (docs/design/config.md D18): the
    // per-run host directory holding `waypipe.sock`, bound rw at ITSELF
    // — right after the state-dirs binds, before every configured
    // mount, so the same later-wins rules treat it as the implicit
    // infrastructure it is. The directory holds exactly one socket file
    // and nothing else (the CLI creates it fresh per run), so the rw
    // bind exposes no host content beyond the channel; both ends of
    // the channel are binaries from mysbx's own closure. The display
    // socket itself (`$XDG_RUNTIME_DIR/wayland-0` below the tmpfs
    // home) is created by the in-sandbox waypipe server, never bound.
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
        bind(&mut argv, false, src, Some(dest));
    }

    // Review-3 item 3: a writable bind may never expose a trusted
    // policy file — the user config (host-wide grants) or the sidecar
    // config (this repo's own sandbox policy). The payload writing one
    // steers the NEXT run: a `git-dirs` approval can be added, the
    // `.git` pointer rewritten to match. `rw` mounts are the direct
    // case; the workspace bind and the git dirs are `rw` too, so they
    // are checked as well — a sidecar or user config sitting inside the
    // work tree is refused, not silently exposed. The worktrees
    // sibling is an rw implicit bind as well and joins the set for the
    // same reason. `ro` mounts do not count: the payload cannot write
    // through them.
    //
    // In a CLONE run the implicit set is exactly the clone (D4: it is
    // the only writable bind), and no configured mount can join it:
    // every `[[mounts]]` entry is downgraded to read-only below, so
    // the policy-file refusal holds by the existing check against
    // the clone's source alone — the bind is exactly the clone
    // directory (D3, "policy-file adjacency"), covering neither
    // `config.toml` nor `state/`.
    for src in cfg
        .mounts
        .iter()
        .filter(|m| m.mode == Mode::Rw && matches!(params.workspace, Workspace::Live))
        .map(|m| normalize(&m.path))
        .chain(implicit_rw_sources.into_iter())
    {
        for policy in params.policy_paths {
            // Every guarded path of the policy, not just its resolved
            // target (review-4 item 1): the directory entries the next
            // run traverses decide WHICH file it reads, so a writable
            // source covering one of them is as good as a writable
            // policy file.
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
        // the workspace bind is checked too (review-3 item 2 said so
        // explicitly — in a clone run that is the CLONE's source,
        // D3). In practice a repo cannot sit there — `/` and the home
        // tree are refused at discovery — but `/nix` or `/nix/var` are
        // ordinary directories, and the rule is cheap.
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
    // The implicit binds the hidden-mount check guards are the
    // workspace bind (the repo in a live run, the clone at the
    // repo's path in a clone run — the DEST is the repo path either
    // way, D3) plus the git dirs, the worktrees sibling and the
    // state binds of a LIVE run; a clone run binds none of the
    // latter three (D3/D4).
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
        // 5a. the forced-ro downgrade of a clone run (workspace.md
        // D4): every `[[mounts]]` entry — rw ones included — is bound
        // read-only, because the clone is the only writable bind of
        // a clone run. The downgrade is silent here but not in the
        // report (`--verbose` marks every downgraded mount) and
        // never applies to the `--rw` flag: that one is REFUSED for a
        // clone run before the argv is built, naming the flag and
        // the mode — an operator who believes a directory is
        // writable while the payload meets `EROFS` is the worse
        // failure.
        let ro = m.mode == Mode::Ro || matches!(params.workspace, Workspace::Clone { .. });
        bind(&mut argv, ro, &m.path, m.dest.as_deref());
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
    // `XDG_RUNTIME_DIR` of a `display = "waypipe"` run is
    // infrastructure for the same reason as `HOME` and `PATH`
    // (docs/design/config.md D18): it anchors the guest waypipe
    // server's display socket (`WAYLAND_DISPLAY = "wayland-0"` is a
    // bare name, so the server creates it in the runtime dir), and
    // the value is the tmpfs home this builder created. Set after
    // `[env]`, so a layer that spells it out never reaches the
    // payload. `WAYLAND_DISPLAY` itself is set by the waypipe
    // server for the wrapped payload, not by the argv.
    if cfg.display.is_waypipe() {
        argv.push("--setenv".into());
        argv.push("XDG_RUNTIME_DIR".into());
        argv.push(SANDBOX_HOME.into());
    }
    // The CA-bundle variables are infrastructure for the same reason as
    // `HOME` and `PATH` (bd myconfig-938): they name a path THIS WRAPPER
    // pinned from its own closure — a store path, no host state — so a
    // layer that repointed them at, say, a host-mounted `/etc` would
    // widen the sandbox's trust anchors to whatever the host has there,
    // not configure the run. Set after `[env]`, so an entry spelling
    // them out shows up in `--dry-run` but never reaches the payload.
    // Only set when a bundle is pinned: an unwrapped build has no
    // closure pin, and inventing a path here would point every
    // `SSL_CERT_FILE`-honoring tool at a nonexistent file — worse than
    // the resolver binds alone, which the `/etc/ssl` row already gives
    // the run.
    if let Some(ca_bundle) = params.ca_bundle {
        for (key, value) in [
            ("SSL_CERT_FILE", ca_bundle),
            ("GIT_SSL_CAINFO", ca_bundle),
            ("NIX_SSL_CERT_FILE", ca_bundle),
        ] {
            argv.push("--setenv".into());
            argv.push(key.into());
            argv.push(value.into());
        }
    }
    // `TMUX_TMPDIR` is infrastructure for the same reason (D16/D17): it
    // names a path inside the tmpfs home this builder created, and it
    // is what keeps the tmux socket out of every host-shared location
    // (`/tmp/tmux-<uid>` on the host, another sandbox's sidecar). Set
    // after `[env]`, so a layer that spells it out parses and shows up
    // in `--dry-run` but never reaches the payload — exactly the
    // treatment `HOME` and `PATH` get.
    if mux.starts_a_session() {
        argv.push("--setenv".into());
        argv.push("TMUX_TMPDIR".into());
        argv.push(MUX_SOCKET_DIR.into());
        // Pass the session name to the entry script for tmux session
        // naming. In window mode (Live workspace) no session name is
        // set; in session mode (Clone workspace) extract the name from
        // the clone path `<repo>.mysbx/clones/NAME`.
        if let Workspace::Clone { clone } = params.workspace {
            if let Some(session_name) = clone.file_name().and_then(|n| n.to_str()) {
                argv.push("--setenv".into());
                argv.push("MYSBX_SESSION_NAME".into());
                argv.push(session_name.into());
            }
        }
    }

    // 7. work in the repo.
    argv.push("--chdir".into());
    argv.push(root.clone());

    // 8. the payload, verbatim.
    argv.push("--".into());
    // A `display = "waypipe"` run wraps EVERY payload form (D18): the
    // guest waypipe server presents the fake compositor socket to the
    // payload (it sets `WAYLAND_DISPLAY`) and connects to the client's
    // per-run socket through the rw bind. All of its flags are ROOT
    // options in waypipe's CLI (the subcommand must come first), its
    // multi mode is the one that CREATES the display socket for the
    // payload — and one server-conn child per window the payload
    // opens — and it exits when the payload does. Its own `--`
    // separates its command from its flags, so the payload stays
    // verbatim after it.
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
        // The multiplexer entry REPLACES the shell (cli.md D11): it is
        // the interactive payload, and it execs the multiplexer's
        // attach in the end, so the session is what the operator's
        // terminal is attached to. `unwrap_or` cannot fall back
        // silently — a missing pin was refused above.
        Payload::Shell if mux.starts_a_session() => {
            argv.push(params.mux_entry.unwrap_or(params.shell).into())
        }
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
        /// The guarded path actually exposed: the policy file's
        /// resolved target, or one of the directory entries the next
        /// run traverses to find it (review-4 item 1) — the two are
        /// the same path only when no symlink is involved.
        exposed: String,
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
    /// The configuration selects a multiplexer
    /// (`multiplexer = "…"`, docs/design/config.md D17) but this build
    /// pinned no entry for it (`MYSBX_MUX_ENTRY_*`). Falling back to a
    /// plain shell is not an option: the operator asked for the
    /// session, and a silent bare shell would be noticed only after
    /// the work happened in the wrong place.
    MultiplexerUnavailable { multiplexer: Multiplexer },
    /// A mount `dest` is related to [`MUX_SOCKET_DIR`] (docs/design/
    /// config.md D16/D17): the private socket of a multiplexer payload
    /// must live in the sandbox home tmpfs and nowhere else. A dest AT
    /// the directory would put a host directory under the socket —
    /// making it reachable from the host and from every other sandbox
    /// binding the same path — and a dest BELOW it would land inside
    /// the very directory the multiplexer's server owns.
    MuxSocketDest { dest: String },
    /// A `state-dirs` entry would make [`MUX_SOCKET_DIR`]
    /// sidecar-backed (docs/design/config.md D16/D17): the socket would
    /// then be a path on the HOST, shared by every mysbx sandbox of
    /// this repository, instead of a path that dies with the tmpfs.
    MuxSocketPersisted { entry: String },
    /// The configuration selects the waypipe display
    /// (`display = "waypipe"`, docs/design/config.md D18) but this
    /// build pinned no waypipe (`MYSBX_WAYPIPE`). Falling back to a
    /// headless run is not an option: the operator asked for windows,
    /// and silently not getting them would be discovered only after a
    /// payload that needed the display failed.
    DisplayUnavailable,
    /// A mount `dest` is related to [`WAYPIPE_DISPLAY_PATH`]
    /// (docs/design/config.md D18): the fake compositor socket the
    /// guest waypipe server presents must be created inside the
    /// sandbox home tmpfs by that server, never seeded from a host
    /// mount.
    DisplaySocketDest { dest: String },
    /// A `state-dirs` entry would back the waypipe display socket
    /// (docs/design/config.md D18) with a sidecar directory: the
    /// socket would become a HOST path shared by every sandbox of
    /// this repository instead of dying with the tmpfs home.
    DisplaySocketPersisted { entry: String },
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
            Error::PolicyFileWritable {
                source,
                policy,
                exposed,
            } => write!(
                f,
                "source {source} would expose the policy file {policy} \
                 writable (through {exposed}) — a config the sandbox can \
                 write, or whose pathname it can re-point, steers the NEXT \
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
            Error::MultiplexerUnavailable { multiplexer } => write!(
                f,
                "multiplexer = \"{name}\", but this build pinned no {name} entry \
                 ({var}) \u{2014} the interactive payload would be a \
                 plain shell instead of the session that was asked for \
                 (docs/design/config.md D17); install mysbx with the {name} \
                 integration enabled (myconfig.ai.mysbx.multiplexer and the \
                 matching package option), or select another multiplexer",
                name = multiplexer.name(),
                // Only the unavailable variants reach this arm, and
                // every one of them HAS a pin (`none` needs none and
                // is never refused) — the fallback keeps the
                // formatter total instead of panicking in a
                // diagnostic.
                var = multiplexer.entry_var().unwrap_or("MYSBX_MUX_ENTRY_*"),
            ),
            Error::MuxSocketDest { dest } => write!(
                f,
                "mount dest {dest} is related to the multiplexer's private socket \
                 directory {MUX_SOCKET_DIR}, which mysbx keeps inside the \
                 sandbox home tmpfs so the socket can never be shared with \
                 the host or with another sandbox (docs/design/config.md \
                 D16/D17); mount it elsewhere below {SANDBOX_HOME}"
            ),
            Error::MuxSocketPersisted { entry } => write!(
                f,
                "state-dirs entry `{entry}` would back the multiplexer's private \
                 socket directory {MUX_SOCKET_DIR} with a sidecar directory \u{2014} the \
                 socket would become a HOST path shared by every sandbox of \
                 this repository instead of dying with the tmpfs home \
                 (docs/design/config.md D16/D17); the socket is deliberately not \
                 persistable, so drop the entry"
            ),
            Error::DisplayUnavailable => write!(
                f,
                "display = \"waypipe\" but this build pinned no waypipe client \
                 (MYSBX_WAYPIPE) \u{2014} the payload would run headless instead \
                 of getting its windows (docs/design/config.md D18); install \
                 mysbx on a host that carries waypipe \
                 (myconfig.ai.dev.mysbx.display.package), or set display = \"off\""
            ),
            Error::DisplaySocketDest { dest } => write!(
                f,
                "mount dest {dest} is related to the waypipe display socket \
                 {WAYPIPE_DISPLAY_PATH}, which the in-sandbox waypipe server \
                 creates inside the sandbox home tmpfs \u{2014} a host mount there \
                 would not be the channel mysbx opened \
                 (docs/design/config.md D18); mount it elsewhere below \
                 {SANDBOX_HOME}"
            ),
            Error::DisplaySocketPersisted { entry } => write!(
                f,
                "state-dirs entry `{entry}` would back the waypipe display socket \
                 {WAYPIPE_DISPLAY_PATH} with a sidecar directory \u{2014} the socket \
                 would become a HOST path shared by every sandbox of this \
                 repository instead of dying with the tmpfs home \
                 (docs/design/config.md D18); drop the entry"
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
/// shared (review-1 finding 5) — the resolver path set of
/// `fns/bubblewrap-app.nix`'s `network` combinator, plus `/etc/static`:
/// on NixOS `/etc/ssl` is a symlink farm whose entries point at
/// `/etc/static/ssl/...`, and bwrap resolves only the SOURCE path of a
/// bind, not the symlinks inside it — without the `/etc/static` bind
/// the CA bundle dangles inside the sandbox and TLS is broken (bd
/// myconfig-938; see the call site for the full reasoning and why the
/// mechanism can be a plain `--ro-bind-try` here).
static RESOLVER_PATHS: &[&str] = &[
    "/etc/hosts",
    "/etc/nsswitch.conf",
    "/etc/resolv.conf",
    "/etc/ssl",
    "/etc/static",
    "/run/systemd/resolve",
];

/// The fixed base binds of the MVP (docs/plan.md, base table). Every row
/// with decision "yes" appears exactly once, in the order the existing
/// `fns/bubblewrap-app.nix` base binds them (agents shell out to
/// arbitrary store paths → `/nix/store` first; `/usr/bin/env` shebangs
/// → `/usr/bin`; timezones → `/etc/localtime`; a fresh tmpfs `/tmp`,
/// NOT the host-backed one).
///
/// `/dev/shm` is a tmpfs of its own because bubblewrap's `--dev` does
/// not create one: a payload that allocates POSIX shared memory —
/// a chrome-family browser is the case that forced this — finds the
/// path missing and crashes. It is private to the sandbox like every other tmpfs here,
/// so nothing is shared with the host.
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
        "--tmpfs".into(),
        "/dev/shm".into(),
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
/// `/etc/nix/nix.conf`, `/bin/sh`, `/usr/bin`, `/proc`, `/dev`,
/// `/etc/localtime`,
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
/// `/etc/localtime` or `/run` and so is not refused either. `/etc/static`
/// gets the same treatment (bd myconfig-938): it is a resolver path, and
/// a configured mount whose dest covers it — or lies under it — is
/// allowed to shadow it, exactly like `/etc/ssl`. The deliberate
/// asymmetry with the base binds above is the whole point: a resolver
/// bind is a convenience the host layout decides, not sandbox
/// infrastructure the argv promises in its report, so a layer that
/// mounts its own CA bundle over `/etc/ssl` or `/etc/static` is a
/// configuration choice, not an escape from a base-table guarantee.
static PROTECTED_DESTS: &[&str] = &[
    "/",
    "/nix/store",
    "/nix/var/nix",
    "/etc/nix/nix.conf",
    "/bin/sh",
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

/// The socket isolation of the multiplexer integration (docs/design/
/// config.md D16, generalized by D17), enforced instead of assumed:
/// nothing a configuration can say may move [`MUX_SOCKET_DIR`] out of
/// the sandbox home tmpfs. It applies to EVERY selectable multiplexer,
/// not only to the tmux-based ones — the guard is about the path, and
/// the path is the same one for all of them.
///
/// Two ways a config could:
///
/// - a mount `dest` at the socket directory would bind a HOST
///   directory there, so the socket would live on the host — visible
///   to a tmux client outside the sandbox and to every other sandbox
///   binding the same path; a dest BELOW it would drop foreign content
///   into the directory the tmux server owns. Both directions are
///   refused (`/mysbx-home/.mysbx-tmux2` is a different directory and
///   stays mountable, like `/usr/bin2` for the base paths).
/// - a `state-dirs` entry naming the socket directory (or something
///   inside it) would make it sidecar-backed — a host path again,
///   shared by every sandbox of this repository and surviving the run.
///
/// Only reached for a run that starts a multiplexer: with
/// `multiplexer = "none"` (or the `run` form) there is no socket, and
/// `/mysbx-home/.mysbx-tmux` is an ordinary home path a config may use
/// for anything.
fn check_mux_socket(mounts: &[Mount], state_dirs: &[String]) -> Result<(), Error> {
    let socket = Path::new(MUX_SOCKET_DIR);
    for m in mounts {
        let dest = normalize(m.dest.as_deref().unwrap_or(&m.path));
        if dest.starts_with(socket) || socket.starts_with(&dest) {
            return Err(Error::MuxSocketDest {
                dest: dest.to_string_lossy().into_owned(),
            });
        }
    }
    for entry in state_dirs {
        let dest = normalize(&format!("{SANDBOX_HOME}/{entry}"));
        if dest.starts_with(socket) || socket.starts_with(&dest) {
            return Err(Error::MuxSocketPersisted {
                entry: entry.clone(),
            });
        }
    }
    Ok(())
}

/// The display-socket isolation of a `display = "waypipe"` run
/// (docs/design/config.md D18), enforced like [`check_mux_socket`]:
/// nothing a configuration can say may interfere with the socket the
/// guest waypipe server creates at
/// `$XDG_RUNTIME_DIR/<WAYPIPE_DISPLAY>` (= [`WAYPIPE_DISPLAY_PATH`],
/// below the sandbox home tmpfs).
///
/// Two ways a config could:
///
/// - a mount `dest` at or below the display socket would put host
///   content where the waypipe server must own the directory — a
///   socket planted by a mount would not be the channel mysbx
///   opened. Both directions are refused, like the mux socket dir.
/// - a `state-dirs` entry naming the display socket (or an ancestor
///   of it inside the home) would back it with a sidecar directory,
///   making the display socket a HOST path shared by every sandbox
///   of this repository.
///
/// Only reached for a run that opens the display channel: with
/// `display = "off"` the path is an ordinary home path a config may
/// use for anything.
fn check_display_socket(mounts: &[Mount], state_dirs: &[String]) -> Result<(), Error> {
    let socket = Path::new(WAYPIPE_DISPLAY_PATH);
    for m in mounts {
        let dest = normalize(m.dest.as_deref().unwrap_or(&m.path));
        if dest.starts_with(socket) || socket.starts_with(&dest) {
            return Err(Error::DisplaySocketDest {
                dest: dest.to_string_lossy().into_owned(),
            });
        }
    }
    for entry in state_dirs {
        let dest = normalize(&format!("{SANDBOX_HOME}/{entry}"));
        if dest.starts_with(socket) || socket.starts_with(&dest) {
            return Err(Error::DisplaySocketPersisted {
                entry: entry.clone(),
            });
        }
    }
    Ok(())
}

fn check_hidden_mounts(
    mounts: &[Mount],
    repo_root: &str,
    git_dirs: &[PathBuf],
    worktrees: Option<&Path>,
    state_binds: &[(String, String)],
    waypipe_bind: Option<(&str, &str)>,
) -> Result<(), Error> {
    // Implicit binds come before every configured mount: the repo root,
    // the git metadata directories a `.git` file points at, the
    // workmux worktrees sibling when it exists, and the state binds of
    // a LIVE run; a clone run binds none of the latter three (D3/D4)
    // — plus the waypipe socket bind of a `display = "waypipe"` run
    // (D18), the same implicit-infrastructure treatment.
    let mut implicit: Vec<(PathBuf, &str)> = vec![(normalize(repo_root), "the repo working tree")];
    for g in git_dirs {
        implicit.push((normalize(&g.to_string_lossy()), "a git metadata directory"));
    }
    if let Some(worktrees) = worktrees {
        implicit.push((
            normalize(&worktrees.to_string_lossy()),
            "the worktrees directory",
        ));
    }
    for (_src, dest) in state_binds {
        implicit.push((normalize(dest), "a state directory"));
    }
    if let Some((_src, dest)) = waypipe_bind {
        implicit.push((normalize(dest), "the waypipe socket directory"));
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
            let earlier_dest = normalize(earlier.dest.as_deref().unwrap_or(&earlier.path));
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
    workspace_src: &str,
    workspace_dest: &str,
    git_dirs: &[PathBuf],
    worktrees: Option<&Path>,
    state_binds: &[(String, String)],
    waypipe_bind: Option<(&str, &str)>,
) -> Result<(), Error> {
    // HOST paths whose content the sandbox can write. The workspace
    // bind — the repo in a live run (rw by D13), the session's clone
    // in a clone run (D3/D4, the only writable bind) — the git
    // metadata directories and the worktrees sibling (rw when it
    // exists) start the set; an `rw` mount adds its source, because
    // the payload writes the host path through it.
    let mut writable_sources: Vec<PathBuf> = vec![normalize(workspace_src)];
    writable_sources.extend(git_dirs.iter().map(|g| normalize(&g.to_string_lossy())));
    if let Some(worktrees) = worktrees {
        let w = normalize(&worktrees.to_string_lossy());
        if !writable_sources.contains(&w) {
            writable_sources.push(w);
        }
    }
    // IN-SANDBOX paths below which a dest may not land, because their
    // content is one of the writable sources above. The workspace
    // bind's in-sandbox path is the repo's own path in BOTH modes
    // (D13 for live; D3 binds the clone there, preserving path
    // identity), and the git dirs are bound at their host paths.
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
    let mut writable_dests: Vec<PathBuf> = vec![normalize(workspace_dest)];
    // The git dirs and the worktrees sibling are bound at their own
    // host paths, so their in-sandbox paths are their sources.
    writable_dests.extend(writable_sources.iter().skip(1).cloned());
    for (_src, dest) in state_binds {
        let dest = normalize(dest);
        if !writable_dests.contains(&dest) {
            writable_dests.push(dest);
        }
    }
    // The waypipe socket bind is rw too (D18): the in-sandbox waypipe
    // server writes its connection through it, so its dest joins the
    // in-sandbox writable set and its source the host one — a
    // configured dest may not land below either.
    if let Some((src, dest)) = waypipe_bind {
        let src = normalize(src);
        if !writable_sources.contains(&src) {
            writable_sources.push(src);
        }
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
    use crate::config::{Display, Mount};
    use std::path::PathBuf;

    /// A synthetic repo root. [`bwrap_argv`] is pure: it canonicalizes
    /// nothing and checks no existence, so the synthetic paths need not
    /// exist — and need not be stable across runs.
    fn synth_repo() -> Repo {
        Repo {
            root: PathBuf::from("/synth/repo"),
            sidecar: PathBuf::from("/synth/repo.mysbx"),
            git_dirs: Vec::new(),
            worktrees: None,
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
            forward_env: Vec::new(),
            allow_domains: Vec::new(),
            connect_ports: Vec::new(),
            listen_ports: Vec::new(),
            ssh_key: false,
            multiplexer: Multiplexer::None,
            display: Display::Off,
        }
    }

    fn params() -> Params<'static> {
        Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
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

    /// All `(source, dest)` bind pairs of the argv.
    fn bind_pairs(argv: &[String]) -> Vec<(&str, &str)> {
        argv.windows(3)
            .filter(|w| w[0] == "--ro-bind" || w[0] == "--bind")
            .map(|w| (w[1].as_str(), w[2].as_str()))
            .collect()
    }

    #[test]
    fn sections_in_order() {
        // Byte-for-byte coverage lives in tests/argv.rs; this asserts the
        // SPEC ORDER of the sections (spec "Watch out": a refactor must
        // not reorder sections 3-5).
        let (repo, cfg, p) = shell_repo_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        let n = argv.len();
        assert!(n >= 2);
        assert_eq!(argv[n - 2], "--");
        assert_eq!(argv[n - 1], "/synth/bin/bash");
    }

    #[test]
    fn command_payload_is_verbatim() {
        let (repo, cfg, p) = shell_repo_defaults();
        let payload = Payload::Command(vec!["ls".into(), "-x".into(), "--help".into()]);
        let argv = bwrap_argv(&cfg, &repo, &payload, &HostEnv::new(), &p).unwrap();
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        assert!(argv.contains(&"--share-net".to_string()));

        let mut deny = cfg;
        deny.network = false;
        let argv = bwrap_argv(&deny, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &host, &p).unwrap();
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        let joined = argv.join(" ");
        // No WHOLESALE `/run` bind (the base table's `no` row: D-Bus,
        // the nix-daemon socket, agent sockets). The resolver exception
        // of review-1 finding 5 is narrow and ro: exactly
        // `/run/systemd/resolve`, only when the network is shared.
        assert!(
            !argv
                .windows(3)
                .any(|w| w[0] == "--ro-bind" && w[1] == "/run"),
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        let tmpfs: Vec<&str> = argv
            .windows(2)
            .filter(|w| w[0] == "--tmpfs")
            .map(|w| w[1].as_str())
            .collect();
        assert_eq!(tmpfs, vec!["/dev/shm", "/tmp", SANDBOX_HOME]);
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &host, &p).unwrap();
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
            matches!(
                err,
                Error::ProtectedDest {
                    protected: "/tmp",
                    ..
                }
            ),
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        let cfg_dests: Vec<&str> = argv
            .windows(3)
            .filter(|w| w[0] == "--ro-bind" || w[0] == "--bind")
            .map(|w| w[2].as_str())
            // The repo bind (a --bind of the repo itself, section 4) and
            // the base binds are not this test's subject.
            .filter(|d| {
                !d.starts_with("/synth/repo")
                    && *d != "/nix/store"
                    && *d != "/usr/bin"
                    && *d != "/etc/localtime"
            })
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

    // ---- the multiplexer (docs/design/config.md D17, cli.md D11) -------

    /// The defaults with `multiplexer = "workmux"` and an entry pinned.
    fn mux_defaults() -> (Repo, Merged, Params<'static>) {
        let (repo, mut cfg, mut p) = shell_repo_defaults();
        cfg.multiplexer = Multiplexer::Workmux;
        p.mux_entry = Some("/synth/bin/mysbx-workmux-entry");
        (repo, cfg, p)
    }

    #[test]
    fn a_multiplexer_replaces_the_interactive_shell_and_pins_the_socket_dir() {
        let (repo, cfg, p) = mux_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        let n = argv.len();
        assert_eq!(argv[n - 2], "--");
        assert_eq!(argv[n - 1], "/synth/bin/mysbx-workmux-entry");
        // The socket directory is exported, and it is INSIDE the
        // sandbox home tmpfs — the whole isolation claim of D16/D17.
        let i = pos(&argv, "TMUX_TMPDIR");
        assert_eq!(argv[i + 1], MUX_SOCKET_DIR);
        assert!(
            MUX_SOCKET_DIR.starts_with(&format!("{SANDBOX_HOME}/")),
            "the socket dir must live below the sandbox home"
        );
        // Infrastructure, like HOME and PATH: emitted after `[env]`, so
        // no layer can repoint it.
        assert!(pos(&argv, "PATH") < i);
    }

    #[test]
    fn the_mux_socket_dir_is_never_a_host_path() {
        // No bind may put host content at or below the socket
        // directory, and the host's own tmux socket directories are not
        // bound at all (there is no `/run` and `/tmp` is a tmpfs).
        let (repo, cfg, p) = mux_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        for w in argv.windows(3) {
            if w[0] == "--bind" || w[0] == "--ro-bind" || w[0] == "--ro-bind-try" {
                assert!(
                    !w[2].starts_with(MUX_SOCKET_DIR),
                    "a bind lands in the socket dir: {w:?}"
                );
                assert!(
                    !w[1].starts_with("/tmp/tmux-") && w[1] != "/run",
                    "a host tmux socket location is bound: {w:?}"
                );
            }
        }
        // `/tmp` is a tmpfs, not a bind — so the host's default
        // `/tmp/tmux-<uid>` cannot be reached even by accident.
        assert!(argv.windows(2).any(|w| w[0] == "--tmpfs" && w[1] == "/tmp"));
    }

    #[test]
    fn the_multiplexer_does_not_touch_the_run_form() {
        // cli.md D11: `run -- CMD` is a one-shot; the argv must be
        // byte-identical to the `multiplexer = "none"` one.
        let (repo, cfg, p) = mux_defaults();
        let payload = Payload::Command(vec!["ls".into()]);
        let with = bwrap_argv(&cfg, &repo, &payload, &HostEnv::new(), &p).unwrap();
        let mut off = cfg.clone();
        off.multiplexer = Multiplexer::None;
        let without = bwrap_argv(&off, &repo, &payload, &HostEnv::new(), &p).unwrap();
        assert_eq!(with, without);
        assert!(!with.contains(&"TMUX_TMPDIR".to_string()));
    }

    #[test]
    fn a_multiplexer_without_a_pinned_entry_is_refused() {
        let (repo, mut cfg, p) = shell_repo_defaults();
        cfg.multiplexer = Multiplexer::Workmux; // nothing pinned in `p`
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::MultiplexerUnavailable { .. }),
            "wrong error: {err}"
        );
        // A `run` payload is unaffected: it starts no session, so it
        // needs no entry.
        bwrap_argv(
            &cfg,
            &repo,
            &Payload::Command(vec!["ls".into()]),
            &HostEnv::new(),
            &p,
        )
        .expect("the run form does not need the entry");
    }

    #[test]
    fn a_mount_may_not_land_on_the_mux_socket_dir() {
        for dest in [
            MUX_SOCKET_DIR,
            &format!("{MUX_SOCKET_DIR}/socket"),
            &format!("{MUX_SOCKET_DIR}/../.mysbx-tmux"),
        ] {
            let (repo, mut cfg, p) = mux_defaults();
            cfg.mounts = vec![Mount {
                path: "/synth/data".into(),
                dest: Some(dest.to_string()),
                mode: Mode::Ro,
            }];
            let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
                .expect_err("must be refused");
            assert!(
                matches!(err, Error::MuxSocketDest { .. }),
                "{dest}: wrong error: {err}"
            );
        }
        // A component look-alike is a different directory and stays
        // mountable (same rule as `/usr/bin2` for the base paths).
        let (repo, mut cfg, p) = mux_defaults();
        cfg.mounts = vec![Mount {
            path: "/synth/data".into(),
            dest: Some(format!("{MUX_SOCKET_DIR}2")),
            mode: Mode::Ro,
        }];
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
    }

    #[test]
    fn the_mux_socket_dir_cannot_be_persisted_in_the_sidecar() {
        for entry in [".mysbx-tmux", ".mysbx-tmux/sub"] {
            let (repo, mut cfg, p) = mux_defaults();
            cfg.state_dirs = vec![entry.to_string()];
            let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
                .expect_err("must be refused");
            assert!(
                matches!(err, Error::MuxSocketPersisted { .. }),
                "{entry}: wrong error: {err}"
            );
        }
        // An unrelated state dir stays fine.
        let (repo, mut cfg, p) = mux_defaults();
        cfg.state_dirs = vec![".local/share/opencode".to_string()];
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
    }

    #[test]
    fn multiplexer_none_leaves_the_socket_dir_an_ordinary_home_path() {
        // The guards exist for a run that starts a session; without
        // one, `/mysbx-home/.mysbx-tmux` is just a path below the home
        // and a config may use it (D16: nothing is reserved globally).
        let (repo, mut cfg, p) = shell_repo_defaults();
        cfg.mounts = vec![Mount {
            path: "/synth/data".into(),
            dest: Some(MUX_SOCKET_DIR.to_string()),
            mode: Mode::Ro,
        }];
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        // Same for a state dir of that name (declared alone: the
        // pre-existing rule that a mount may not cover a state bind is
        // a different guard).
        let (repo, mut cfg, p) = shell_repo_defaults();
        cfg.state_dirs = vec![".mysbx-tmux".to_string()];
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
    }

    // ---- the display channel (docs/design/config.md D18) ------------------

    /// The defaults with `display = "waypipe"` and the waypipe pin set.
    fn display_defaults() -> (Repo, Merged, Params<'static>) {
        let (repo, mut cfg, mut p) = shell_repo_defaults();
        cfg.display = Display::Waypipe;
        p.waypipe = Some(Waypipe {
            socket_dir: "/synth/repo.mysbx/waypipe/1234",
            guest_bin: "/synth/bin/waypipe",
        });
        (repo, cfg, p)
    }

    #[test]
    fn a_waypipe_display_binds_the_socket_dir_and_wraps_the_payload() {
        let (repo, cfg, p) = display_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        // The socket directory is bound rw right after the state binds
        // — before every configured mount — at itself.
        assert!(bind_pairs(&argv).contains(&(
            "/synth/repo.mysbx/waypipe/1234",
            "/synth/repo.mysbx/waypipe/1234"
        )));
        // `XDG_RUNTIME_DIR` is infrastructure, set after HOME/PATH.
        let i = pos(&argv, "XDG_RUNTIME_DIR");
        assert_eq!(
            &argv[i - 1..i + 2],
            &["--setenv", "XDG_RUNTIME_DIR", SANDBOX_HOME]
        );
        assert!(pos(&argv, "HOME") < i && pos(&argv, "PATH") < i);
        // The payload is wrapped in the guest waypipe server — its
        // flags are root options (before the `server` subcommand), its
        // multi mode creates the display socket — which presents
        // `wayland-0` to the shell behind its own `--`.
        let dd = pos(&argv, "--");
        assert_eq!(
            &argv[dd + 1..dd + 9],
            &[
                "/synth/bin/waypipe",
                "--socket",
                "/synth/repo.mysbx/waypipe/1234/waypipe.sock",
                "--display",
                WAYPIPE_DISPLAY,
                "server",
                "--",
                "/synth/bin/bash",
            ]
        );
    }

    #[test]
    fn the_waypipe_display_wraps_the_run_form_too() {
        // D18: unlike the multiplexer the display is not a session —
        // a one-shot `run -- CMD` whose command opens a window needs
        // the channel, so it is wrapped like the shell.
        let (repo, cfg, p) = display_defaults();
        let payload = Payload::Command(vec!["ls".into(), "-x".into()]);
        let argv = bwrap_argv(&cfg, &repo, &payload, &HostEnv::new(), &p).unwrap();
        let dd = argv.iter().rposition(|x| x == "--").unwrap();
        assert_eq!(&argv[dd + 1..], &["ls", "-x"]);
        assert_eq!(argv[dd - 1], "server");
    }

    #[test]
    fn a_waypipe_display_without_the_pin_is_refused() {
        let (repo, mut cfg, p) = shell_repo_defaults();
        cfg.display = Display::Waypipe; // nothing pinned in `p`
        let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
            .expect_err("must be refused");
        assert!(
            matches!(err, Error::DisplayUnavailable),
            "wrong error: {err}"
        );
        let err = bwrap_argv(
            &cfg,
            &repo,
            &Payload::Command(vec!["ls".into()]),
            &HostEnv::new(),
            &p,
        )
        .expect_err("the run form needs the channel too");
        assert!(
            matches!(err, Error::DisplayUnavailable),
            "wrong error: {err}"
        );
    }

    #[test]
    fn a_mount_may_not_land_on_the_waypipe_display_socket() {
        for dest in [
            WAYPIPE_DISPLAY_PATH,
            &format!("{WAYPIPE_DISPLAY_PATH}/sub"),
            &format!("{WAYPIPE_DISPLAY_PATH}/../wayland-0"),
        ] {
            let (repo, mut cfg, p) = display_defaults();
            cfg.mounts = vec![Mount {
                path: "/synth/data".into(),
                dest: Some(dest.to_string()),
                mode: Mode::Ro,
            }];
            let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
                .expect_err("must be refused");
            assert!(
                matches!(err, Error::DisplaySocketDest { .. }),
                "{dest}: wrong error: {err}"
            );
        }
    }

    #[test]
    fn the_waypipe_display_socket_may_not_be_persisted() {
        for entry in ["wayland-0", "wayland-0/x"] {
            let (repo, mut cfg, p) = display_defaults();
            cfg.state_dirs = vec![entry.to_string()];
            let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p)
                .expect_err("must be refused");
            assert!(
                matches!(err, Error::DisplaySocketPersisted { .. }),
                "{entry}: wrong error: {err}"
            );
        }
        // An unrelated state dir stays fine.
        let (repo, mut cfg, p) = display_defaults();
        cfg.state_dirs = vec![".local/share/opencode".to_string()];
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
    }

    #[test]
    fn display_off_leaves_the_display_path_an_ordinary_home_path() {
        // The guards exist for a run that opens the channel; with
        // `display = "off"` the display path is just a home path a
        // config may use (D18: nothing is reserved globally).
        let (repo, mut cfg, p) = shell_repo_defaults();
        cfg.mounts = vec![Mount {
            path: "/synth/data".into(),
            dest: Some(WAYPIPE_DISPLAY_PATH.to_string()),
            mode: Mode::Ro,
        }];
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
        let (repo, mut cfg, p) = shell_repo_defaults();
        cfg.state_dirs = vec!["wayland-0".to_string()];
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p).unwrap();
    }
}
