// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The `--verbose` run report (docs/design/cli.md D10).
//!
//! One pure function over the values the pipeline already computed:
//! the resolved repository, the two configuration layers with the paths
//! they were loaded from, the merged result, the runtime parameters
//! (`MYSBX_*` after their fallbacks) and the payload. It performs no I/O
//! and reads no environment — the caller passes everything in, so the
//! report can be asserted line by line in a unit test.
//!
//! Every line carries the `## ` stdout prefix (cli.md D9), which is what
//! keeps the report separable from the `--dry-run` argv: the argv block
//! is the only unprefixed thing on stdout, so
//! `mysbx --verbose --dry-run | grep -v '^## '` is byte-identical to the
//! plain `mysbx --dry-run` output.
//!
//! `[env]` values are printed **verbatim, not redacted** (cli.md D10):
//! `--dry-run` already prints them as `--setenv KEY VALUE`, so hiding
//! them here would buy no secrecy while making the report lie about the
//! run. Whoever can run `mysbx --verbose` can also read both config
//! files.

use crate::bwrap::{
    HostEnv, Params, Payload, Workspace, MUX_SOCKET_DIR, SANDBOX_HOME, WAYPIPE_DISPLAY,
};
use crate::config::{Display, Mode};
use crate::merge::Merged;
use crate::repo::Repo;
use std::path::Path;

/// Everything the report shows. Borrowed, so building it costs nothing
/// and cannot drift from the values the pipeline actually uses.
pub struct Report<'a> {
    pub repo: &'a Repo,
    /// Whether the sidecar directory exists. On a sandbox run this is
    /// always true (cli.md D13: an uninitialized repo is refused before
    /// the report is built); the flag stays because the renderer is
    /// used by tests with synthetic reports too.
    /// The report cannot say *how* the repo was
    /// resolved — `repo::resolve` does not return that — so it says what
    /// it does know: the two paths and whether the sidecar is there.
    pub sidecar_exists: bool,
    /// The user configuration path `load_layers` used, and whether that
    /// file exists (an absent file is an empty layer, not an error).
    pub user_config: &'a Path,
    pub user_config_exists: bool,
    pub sidecar_config: &'a Path,
    pub sidecar_config_exists: bool,
    /// The merged, effective configuration.
    pub merged: &'a Merged,
    /// Whether `merged.backend` was set by `--backend` for THIS run
    /// (cli.md D18, bd myconfig-veg) instead of arriving from a config
    /// layer. The backend line's provenance tag — the report marks
    /// every command-line contribution (`[command line]` mounts,
    /// D16) so it never claims a decision came from a file the
    /// operator never opened.
    pub backend_from_cli: bool,
    /// How many of `merged.mounts` came from the user layer. The merge
    /// puts the user mounts first, in declaration order, then the
    /// accepted sidecar mounts — so this single number attributes every
    /// mount to its layer without re-deriving the merge.
    pub user_mount_count: usize,
    /// How many of `merged.mounts` came after both config layers —
    /// the `--ro`/`--rw` additions of the command line (cli.md D16),
    /// appended by the pipeline after the merge. The last
    /// `cli_mount_count` mounts are attributed to the command line,
    /// the ones before them split at `user_mount_count`.
    pub cli_mount_count: usize,
    /// The forwarded host variables (already filtered to the ones that
    /// were actually set).
    pub host_env: &'a HostEnv,
    pub params: &'a Params<'a>,
    /// The backend binary that would be executed (`MYSBX_BWRAP` /
    /// `MYSBX_PODMAN` / `MYSBX_NONO` after their fallbacks).
    pub bwrap_bin: &'a str,
    /// The configured backend name (`bubblewrap`, `podman-gvisor` or
    /// `nono`). The report labels the backend line with it (`bwrap:` /
    /// `podman:` / `nono:`) — the label is what an operator greps for, and a
    /// nono run claiming a `bwrap:` binary would lie.
    pub backend: &'a str,
    /// The container image of a podman-gvisor run
    /// (`MYSBX_GVISOR_IMAGE`). `None` on bubblewrap, where the line
    /// is not printed at all — a bwrap run has no image, and printing
    /// `(none)` for it would suggest a missing pin instead of an
    /// inapplicable one.
    pub image: Option<&'a str>,
    pub payload: &'a Payload,
    /// Whether this run stops before `exec` — said out loud, so the
    /// report never claims a command ran that did not.
    pub dry_run: bool,
    /// Whether this run waits and records a result (cli.md D17, bd
    /// myconfig-0ql) instead of exec'ing — the `mode:` line must not
    /// say "executing" for a run whose outcome is interpreted.
    pub result: bool,
}

/// Render the report as `## `-prefixed stdout lines, without trailing
/// newlines (the caller prints them one per line).
pub fn lines(r: &Report<'_>) -> Vec<String> {
    let mut out = Vec::new();
    let mut p = |s: String| out.push(format!("## {s}"));

    p(format!("mysbx {} — run configuration", crate::VERSION));
    p(format!("repo root:      {}", r.repo.root.display()));
    // Review-1 finding 4: git metadata a `.git` FILE points at is bound
    // rw into the sandbox — it must be said out loud, like the rest of
    // the run configuration, because it is host state outside the repo
    // the sandbox can write.
    for g in &r.repo.git_dirs {
        p(format!("git metadata:   {} (bound rw)", g.display()));
    }
    // The worktrees sibling is an implicit rw bind too, discovered per
    // run (see [`crate::repo::Repo::worktrees`]): what the report must
    // say is THAT it is bound and WHY the operator never wrote a
    // `[[mounts]]` entry for it.
    if let Some(worktrees) = &r.repo.worktrees {
        p(format!(
            "worktrees:      {} (bound rw, implicit — exists next to the repo)",
            worktrees.display()
        ));
    }
    p(format!(
        "sidecar:        {} ({})",
        r.repo.sidecar.display(),
        if r.sidecar_exists {
            "exists"
        } else {
            "missing"
        }
    ));
    p(format!(
        "user config:    {} ({})",
        r.user_config.display(),
        present(r.user_config_exists)
    ));
    p(format!(
        "sidecar config: {} ({})",
        r.sidecar_config.display(),
        present(r.sidecar_config_exists)
    ));
    p(format!(
        "backend:        {}{}",
        r.merged.backend.as_deref().unwrap_or("(none)"),
        if r.backend_from_cli {
            "  [--backend]"
        } else {
            ""
        }
    ));
    p(format!(
        "network:        {}",
        if r.merged.network {
            "shared (--share-net)"
        } else {
            "denied (--unshare-all, no --share-net)"
        }
    ));

    // The workspace of this run (workspace.md D1): the live repo —
    // the default, said out loud like the multiplexer is, because
    // "which tree does this run write" is a property of every run —
    // or the named session's clone (D3: bound rw at the repo's own
    // path, host repo not mounted, every configured mount forced ro
    // by D4). The clone-creation state is the caller's business; the
    // report describes the run as it stands.
    match r.params.workspace {
        Workspace::Live => {
            p("workspace:      live — the repo itself, bound rw (the default)".to_string())
        }
        Workspace::Clone { clone } => p(format!(
            "workspace:      clone run — {} bound rw at {} (the host repo is not mounted)",
            clone.display(),
            r.repo.root.display(),
        )),
    }

    // Mounts, in argv order: the implicit workspace bind first (the
    // repo in a live run, config.md D13; the session's clone at the
    // repo's own path in a clone run, workspace.md D3), then the
    // worktrees sibling (implicit, when it exists — a live run only,
    // D3), then the configured mounts in declaration order. In a
    // clone run every configured mount is forced ro (D4), and the
    // report marks every downgraded entry — the operator must see
    // which entries the mode narrowed, not just a changed letter.
    let clone_run = matches!(r.params.workspace, Workspace::Clone { .. });
    p(format!(
        "mounts:         {} (in declaration order)",
        r.merged.mounts.len()
            + 1
            + if clone_run {
                0
            } else {
                r.repo.worktrees.iter().count()
            }
    ));
    match r.params.workspace {
        Workspace::Live => p(format!(
            "  rw {} -> {}  [repo, implicit]",
            r.repo.root.display(),
            r.repo.root.display()
        )),
        Workspace::Clone { clone } => p(format!(
            "  rw {} -> {}  [session clone, implicit]",
            clone.display(),
            r.repo.root.display()
        )),
    }
    if let (false, Some(worktrees)) = (clone_run, &r.repo.worktrees) {
        p(format!(
            "  rw {} -> {}  [worktrees, implicit]",
            worktrees.display(),
            worktrees.display()
        ));
    }
    for (i, m) in r.merged.mounts.iter().enumerate() {
        let layer = if i < r.user_mount_count {
            "user config"
        } else if i + r.cli_mount_count >= r.merged.mounts.len() {
            "command line"
        } else {
            "sidecar config"
        };
        // D4: a clone run downgrades every rw entry to ro. The report
        // shows the EFFECTIVE mode and marks the downgrade, so a
        // configured `mode = "rw"` never silently reads as if it
        // applied.
        let (mode, downgrade) = if clone_run && m.mode == Mode::Rw {
            ("ro", ", downgraded — clone run")
        } else {
            (
                match m.mode {
                    Mode::Ro => "ro",
                    Mode::Rw => "rw",
                },
                "",
            )
        };
        p(format!(
            "  {mode} {} -> {}  [{layer}{downgrade}]",
            m.path,
            m.dest.as_deref().unwrap_or(&m.path),
        ));
    }

    // The sandbox's own home (config.md D14): a tmpfs, not a bind, so it
    // is not part of the mount list above — but an operator reading the
    // report must see where `$HOME` points and that it is not the host's.
    // State dirs (D15) bind subdirectories of it to sidecar-backed
    // stores, so with any declared entry the parenthetical says so
    // instead of implying an all-ephemeral home — EXCEPT in a clone
    // run, where the state-dirs are not handled at all (workspace.md
    // D4): the home stays all-ephemeral and the report must not claim
    // a persistence the run does not perform.
    //
    // Under nono NONE of that holds (no tmpfs home, no remap): HOME is
    // the real host home — kept unwritable by Landlock — and the state
    // stores persist at their real sidecar paths, so the line must
    // say the backend's actual semantics instead of bwrap's.
    if r.backend == "nono" {
        if r.merged.state_dirs.is_empty() {
            p("home:           the host home is $HOME (no remap; the host home is not writable under nono)".to_string());
        } else {
            p(format!(
                "home:           the host home is $HOME (no remap; the host home is not writable under nono; {} state dir(s) persist at their sidecar paths)",
                r.merged.state_dirs.len(),
            ));
        }
    } else if r.merged.state_dirs.is_empty() || clone_run {
        p(format!(
            "home:           {SANDBOX_HOME} (tmpfs; the host home is not mounted)"
        ));
    } else {
        p(format!(
            "home:           {SANDBOX_HOME} (tmpfs + {} state dir(s) persisted in the sidecar; the host home is not mounted)",
            r.merged.state_dirs.len(),
        ));
    }

    // State dirs (config.md D15), in declaration order: what the sandbox
    // persists across runs and where the backing store lives. They are
    // implicit binds like the repo, so they belong with the mount
    // listing's provenance, not buried in prose. In a clone run they
    // are OFF (workspace.md D4) — said out loud when a layer declared
    // any, so the difference to a live run is visible instead of
    // implied by absence.
    if !r.merged.state_dirs.is_empty() && clone_run {
        p(format!(
            "state dirs:     off in a clone run ({} declared, not handled — workspace.md D4)",
            r.merged.state_dirs.len()
        ));
    }
    if !r.merged.state_dirs.is_empty() && !clone_run {
        p(format!(
            "state dirs:     {} (rw, persisted in the sidecar)",
            r.merged.state_dirs.len()
        ));
        // The in-sandbox path in full (`/mysbx-home/<entry>`), not the
        // bare entry: the report is read against the argv, where the
        // dest is spelled out, and `/<entry>` would read like a path at
        // the sandbox root. Under nono there is NO remap: the store is
        // reachable at its sidecar path and nowhere else, so the line
        // says that instead of inventing a sandbox path.
        for entry in &r.merged.state_dirs {
            if r.backend == "nono" {
                p(format!(
                    "  {}  [state; no remap — the sandbox path is the sidecar path]",
                    r.repo.sidecar.join("state").join(entry).display()
                ));
            } else {
                p(format!(
                    "  {SANDBOX_HOME}/{entry} <-> {}  [state]",
                    r.repo.sidecar.join("state").join(entry).display()
                ));
            }
        }
    }

    // Environment. Values are shown verbatim; see the module docs for
    // why they are not redacted.
    p(format!(
        "env:            {} forwarded from the host, {} from [env] (values shown verbatim — they may be secrets)",
        r.host_env.len(),
        r.merged.env.len(),
    ));
    // Under nono the parent environment is INHERITED (nono has no
    // `--clearenv` equivalent; bwrap clears, podman passes flags) —
    // the forwarded and `[env]` values above are pinned ON TOP of it,
    // so the count is not the whole environment the payload sees.
    if r.backend == "nono" {
        p("  (nono inherits the parent environment; the forwarded and [env] values above are pinned on top)".to_string());
    }
    let display_on = r.merged.display.is_waypipe();
    for (k, v) in r.host_env {
        // Both sections are printed in argv order (host first, `[env]`
        // second), and the argv builder lets the later `--setenv` win —
        // so a host variable that `[env]` also sets never reaches the
        // payload. Say so, instead of listing the same name twice with
        // two values and no hint which one applies.
        if infrastructure(k, display_on) {
            p(format!("  {k}={v}  [host, ignored — set by mysbx]"));
        } else if r.merged.env.contains_key(k) {
            p(format!("  {k}={v}  [host, overridden by [env]]"));
        } else {
            p(format!("  {k}={v}  [host]"));
        }
    }
    for (k, v) in &r.merged.env {
        if infrastructure(k, display_on) {
            // `HOME` and `PATH` are set after `[env]` (config.md D14), so
            // a layer that names them never reaches the payload.
            p(format!("  {k}={v}  [config, ignored — set by mysbx]"));
        } else {
            p(format!("  {k}={v}  [config]"));
        }
    }
    // `HOME` differs by backend: bwrap and podman create a sandbox
    // home and point HOME at it; nono has no remap and HOME stays
    // the real host home (kept unwritable by Landlock) — said here
    // too, so the env block agrees with the `home:` line above.
    if r.backend == "nono" {
        p("  HOME=$HOME of the invoking user  [host home — no remap]".to_string());
    } else {
        p(format!("  HOME={SANDBOX_HOME}  [sandbox home]"));
    }
    p(format!("  PATH={}  [tools]", r.params.tools_path));
    // The pinned CA bundle belongs in the report for the same reason
    // as the nix.conf and `/bin/sh` lines below: which trust anchors
    // the sandbox's TLS tools use — the wrapper's own `nss-cacert`, or
    // whatever the host `/etc` layout serves via the resolver binds —
    // is a property of the run (bd myconfig-938).
    for (k, v) in [
        ("SSL_CERT_FILE", r.params.ca_bundle),
        ("GIT_SSL_CAINFO", r.params.ca_bundle),
        ("NIX_SSL_CERT_FILE", r.params.ca_bundle),
    ] {
        if let Some(b) = v {
            p(format!("  {k}={b}  [pinned CA bundle]"));
        }
    }

    // The backend binary, labeled by the backend itself: `bwrap:` for
    // the bubblewrap backend, `podman:` for podman-gvisor (which also
    // names its image — the container the run starts is as much a
    // property of the run as the binary a bwrap run execs), `nono:`
    // for the nono backend (a host-path backend like bwrap — the
    // report's shell/PATH/nix.conf//bin/sh/ca-bundle pins describe
    // the exec environment lib.rs sets on top of the inherited
    // parent env, not argv flags).
    match r.backend {
        "podman-gvisor" => {
            p(format!("podman:         {}", r.bwrap_bin));
            if let Some(image) = r.image {
                p(format!("image:          {image}"));
            }
        }
        "nono" => p(format!("nono:           {}", r.bwrap_bin)),
        _ => p(format!("bwrap:          {}", r.bwrap_bin)),
    }
    p(format!("shell:          {}", r.params.shell));
    p(format!("tools PATH:     {}", r.params.tools_path));
    // Review-2 item 3: the host's /etc/nix/nix.conf is never mounted
    // (it may hold access-tokens); what the sandbox sees is the
    // generated, credential-free file the wrapper pins — or nothing.
    // Both states belong in the report: "nix reads no configuration"
    // is as much a property of the run as which shell it starts.
    // Under podman-gvisor there is no nix inside the sandbox at all
    // (the image ships none; a writable-store mechanism like the
    // gvisor tier's `--nix` volume is deliberately out of scope for
    // this backend), so the line says that instead of implying nix
    // would merely read its defaults.
    p(format!(
        "nix.conf:       {}",
        match (r.backend, r.params.nix_conf) {
            ("podman-gvisor", _) => "(none — no nix inside the sandbox)",
            (_, Some(conf)) => conf,
            (_, None) => "(none — nix uses its defaults)",
        }
    ));
    // The `/bin/sh` state belongs in the report for the same reason
    // as the nix.conf line above: whether tmux `run-shell` jobs and
    // `#!/bin/sh` shebangs can run at all inside this sandbox is a
    // property of the run, not packaging detail — an unwrapped build
    // (or a host that pins nothing) gets a sandbox without `/bin/sh`,
    // and the failing hooks that follow are diagnosable from here.
    // Under podman-gvisor there is no `/bin/sh` pin at all: the image
    // provides its own (bd myconfig-wao), so the line says so instead
    // of claiming the sandbox has none.
    p(format!(
        "/bin/sh:        {}",
        match (r.backend, r.params.bin_sh) {
            ("podman-gvisor", _) => "(the container image's own /bin/sh)",
            (_, Some(sh)) => sh,
            (_, None) => "(none — no /bin/sh inside the sandbox)",
        }
    ));
    // The CA bundle is a pin like `MYSBX_NIX_CONF`: named in the
    // report both ways — the pinned store path when the wrapper set
    // one, the honest absence when it did not (the run then relies on
    // the resolver binds of `/etc/ssl` + `/etc/static` alone).
    // Under podman-gvisor there is no pin either: the image carries
    // its own bundle in its OCI env (bd myconfig-wao).
    p(format!(
        "ca-bundle:      {}",
        match (r.backend, r.params.ca_bundle) {
            ("podman-gvisor", _) => "(the container image's own CA bundle)",
            (_, Some(b)) => b,
            (_, None) => "(none — TLS trust anchors come from the /etc/ssl bind)",
        }
    ));
    // The multiplexer (config.md D17 / cli.md D11): which one was
    // selected, what replaces the shell, where its private socket
    // lives, and — for the `run` form — that this run keeps the plain
    // payload after all. Saying the socket path out loud is the point:
    // "the socket is inside the sandbox" is the isolation claim, and
    // the report is where an operator checks it against the argv.
    // Printed for `none` too: which payload an interactive run starts
    // is a property of every run, and the value now has six possible
    // answers rather than a silent "off".
    let mux = r.merged.multiplexer;
    if !mux.starts_a_session() {
        p("multiplexer:    none — no session is started".to_string());
    } else {
        match r.payload {
            Payload::Shell => {
                p(format!(
                    "multiplexer:    {mux} — private socket dir {MUX_SOCKET_DIR} (inside the sandbox home tmpfs; no host tmux server is reachable)"
                ));
                p(format!(
                    "  entry:        {}",
                    r.params
                        .mux_entry
                        .unwrap_or("(none pinned — the run is refused)")
                ));
            }
            Payload::Command(_) => p(format!(
                "multiplexer:    {mux}, but not applied — it replaces the INTERACTIVE payload only (cli.md D11)"
            )),
        }
    }

    // The display channel (config.md D18): which one a run uses, where
    // its guest socket lands, and — when nothing is pinned — that the
    // run is refused. The host client binary and the in-sandbox socket
    // path are the two facts an operator checks the argv against.
    match r.merged.display {
        Display::Off => p("display:         off — the run is headless".to_string()),
        Display::Waypipe => match r.params.waypipe.as_ref() {
            Some(wp) => {
                p(format!(
                        "display:         waypipe — socket {}/waypipe.sock (host side), guest display {} under {SANDBOX_HOME}",
                        wp.socket_dir,
                        WAYPIPE_DISPLAY,
                    ));
                p(format!("  guest bin:    {}", wp.guest_bin));
            }
            None => p(
                "display:         waypipe, refused — no waypipe pinned (MYSBX_WAYPIPE)".to_string(),
            ),
        },
    }

    match r.payload {
        Payload::Shell if mux.starts_a_session() => p(format!(
            "payload:        {mux} session {}",
            r.params.mux_entry.unwrap_or("(none pinned)")
        )),
        Payload::Shell => p(format!("payload:        shell {}", r.params.shell)),
        // Space-joined for readability only; the exact, unambiguous
        // argument vector is what `--dry-run` prints.
        Payload::Command(args) => p(format!("payload:        command {}", args.join(" "))),
    }
    p(if r.dry_run {
        "mode:           dry run — the argv follows, nothing is executed".into()
    } else if r.result {
        "mode:           waiting for the payload, the outcome is recorded in result.json"
            .to_string()
    } else {
        "mode:           executing".to_string()
    });
    out
}

/// The variables `bwrap_argv` sets last and no layer can override
/// (config.md D14).
fn infrastructure(key: &str, display_on: bool) -> bool {
    // `SSL_CERT_FILE`/`GIT_SSL_CAINFO`/`NIX_SSL_CERT_FILE` are set after
    // `[env]` when a bundle is pinned (bd myconfig-938), so a layer that
    // names them never reaches the payload — the same treatment
    // `HOME` and `PATH` get.
    // `XDG_RUNTIME_DIR` and `WAYLAND_DISPLAY` join the set only for a
    // run whose display is on (config.md D18): the argv pins the first
    // at the sandbox home, and the guest `waypipe server` mysbx wraps
    // the payload in sets the second for it — a later `--setenv`-like
    // win, so a layer that names either never reaches the payload
    // THEN. With the display off neither variable is infrastructure,
    // and a forwarded or configured value reaches the payload like
    // any other.
    key == "HOME"
        || key == "PATH"
        || key == "SSL_CERT_FILE"
        || key == "GIT_SSL_CAINFO"
        || key == "NIX_SSL_CERT_FILE"
        || (display_on && (key == "XDG_RUNTIME_DIR" || key == "WAYLAND_DISPLAY"))
}

fn present(exists: bool) -> &'static str {
    if exists {
        "loaded"
    } else {
        "absent — empty layer"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{Display, Mount, Multiplexer};
    use std::collections::BTreeMap;
    use std::path::PathBuf;

    fn fixture_report() -> (Repo, Merged, HostEnv) {
        let repo = Repo {
            root: PathBuf::from("/synth/repo"),
            sidecar: PathBuf::from("/synth/repo.mysbx"),
            git_dirs: Vec::new(),
            worktrees: None,
        };
        let mut env = BTreeMap::new();
        env.insert("EDITOR".to_owned(), "nvim".to_owned());
        let merged = Merged {
            backend: Some("bubblewrap".into()),
            network: false,
            mounts: vec![
                Mount {
                    path: "/synth/shared".into(),
                    dest: None,
                    mode: Mode::Rw,
                },
                Mount {
                    path: "/synth/shared/sub".into(),
                    dest: Some("/inside".into()),
                    mode: Mode::Ro,
                },
            ],
            env,
            git_dirs: Vec::new(),
            state_dirs: Vec::new(),
            forward_env: Vec::new(),
            allow_domains: Vec::new(),
            connect_ports: Vec::new(),
            listen_ports: Vec::new(),
            multiplexer: Multiplexer::None,
            display: Display::Off,
        };
        let mut host = HostEnv::new();
        host.insert("TERM".to_owned(), "xterm".to_owned());
        (repo, merged, host)
    }

    fn render(dry_run: bool, payload: &Payload) -> Vec<String> {
        let (repo, merged, host) = fixture_report();
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: false,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "/synth/bin/bwrap",
            backend: "bubblewrap",
            image: None,
            payload,
            dry_run,
            result: false,
        })
    }

    #[test]
    fn every_line_carries_the_stdout_prefix() {
        // The property the --dry-run compatibility rests on (cli.md D10):
        // no report line may be mistaken for an argv line.
        for line in render(true, &Payload::Shell) {
            assert!(line.starts_with("## "), "unprefixed: {line}");
        }
    }

    #[test]
    fn reports_paths_backend_network_mounts_env_and_payload() {
        let joined = render(true, &Payload::Command(vec!["ls".into(), "-x".into()])).join("\n");
        assert!(joined.contains("repo root:      /synth/repo"), "{joined}");
        assert!(joined.contains("/synth/repo.mysbx (exists)"), "{joined}");
        assert!(
            joined.contains("/synth/xdg/mysbx/config.toml (loaded)"),
            "{joined}"
        );
        assert!(
            joined.contains("/synth/repo.mysbx/config.toml (absent — empty layer)"),
            "{joined}"
        );
        assert!(joined.contains("backend:        bubblewrap"), "{joined}");
        assert!(joined.contains("network:        denied"), "{joined}");
        // The implicit repo mount plus both configured ones, attributed
        // to their layer, in declaration order.
        assert!(
            joined.contains("rw /synth/repo -> /synth/repo  [repo, implicit]"),
            "{joined}"
        );
        assert!(
            joined.contains("rw /synth/shared -> /synth/shared  [user config]"),
            "{joined}"
        );
        assert!(
            joined.contains("ro /synth/shared/sub -> /inside  [sidecar config]"),
            "{joined}"
        );
        assert!(joined.contains("TERM=xterm  [host]"), "{joined}");
        assert!(joined.contains("EDITOR=nvim  [config]"), "{joined}");
        // The host EDITOR is not shadowed here (the fixture forwards
        // only TERM), so nothing claims an override.
        assert!(!joined.contains("overridden"), "{joined}");
        assert!(joined.contains("PATH=/synth/bin  [tools]"), "{joined}");
        // The sandbox home is reported, and it is not the host's.
        assert!(
            joined.contains(&format!(
                "home:           {SANDBOX_HOME} (tmpfs; the host home is not mounted)"
            )),
            "{joined}"
        );
        assert!(
            joined.contains(&format!("  HOME={SANDBOX_HOME}  [sandbox home]")),
            "{joined}"
        );
        assert!(
            joined.contains("bwrap:          /synth/bin/bwrap"),
            "{joined}"
        );
        assert!(
            joined.contains("shell:          /synth/bin/bash"),
            "{joined}"
        );
        assert!(joined.contains("payload:        command ls -x"), "{joined}");
        assert!(joined.contains("dry run"), "{joined}");
    }

    #[test]
    fn a_host_variable_that_env_also_sets_is_marked_overridden() {
        // The argv sets `[env]` last, so `[env]` wins; the report must
        // not present the host value as if it reached the payload.
        let (repo, merged, mut host) = fixture_report();
        host.insert("EDITOR".to_owned(), "host-vi".to_owned());
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: true,
            result: false,
        })
        .join("\n");
        assert!(
            joined.contains("EDITOR=host-vi  [host, overridden by [env]]"),
            "{joined}"
        );
        assert!(joined.contains("EDITOR=nvim  [config]"), "{joined}");
        assert!(joined.contains("TERM=xterm  [host]"), "{joined}");
    }

    #[test]
    fn an_env_home_is_reported_as_ignored() {
        // config.md D14: `[env] HOME` (or `PATH`) does not reach the
        // payload — the report must not pretend it does.
        let (repo, mut merged, mut host) = fixture_report();
        merged
            .env
            .insert("HOME".to_owned(), "/synth/evil".to_owned());
        host.insert("HOME".to_owned(), "/synth/host-home".to_owned());
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: true,
            result: false,
        })
        .join("\n");
        assert!(
            joined.contains("HOME=/synth/evil  [config, ignored — set by mysbx]"),
            "{joined}"
        );
        assert!(
            joined.contains("HOME=/synth/host-home  [host, ignored — set by mysbx]"),
            "{joined}"
        );
        assert!(
            joined.contains(&format!("  HOME={SANDBOX_HOME}  [sandbox home]")),
            "{joined}"
        );
    }

    #[test]
    fn network_shared_and_exec_mode_are_said_out_loud() {
        let (repo, mut merged, host) = fixture_report();
        merged.network = true;
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: false,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: false,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 2,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: false,
            result: false,
        })
        .join("\n");
        assert!(joined.contains("network:        shared"), "{joined}");
        assert!(joined.contains("mode:           executing"), "{joined}");
        assert!(joined.contains("/synth/repo.mysbx (missing)"), "{joined}");
        assert!(
            joined.contains("payload:        shell /synth/bin/bash"),
            "{joined}"
        );
        // With user_mount_count == 2 both mounts belong to the user layer.
        assert!(!joined.contains("[sidecar config]"), "{joined}");
    }

    #[test]
    fn state_dirs_are_listed_with_their_sidecar_backing_store() {
        // config.md D15: the report must show what persists across runs
        // and where it lives — the entry below the sandbox home and the
        // backing store below the sidecar, one line per entry. With an
        // entry declared, the home line must also stop implying an
        // all-ephemeral tmpfs.
        let (repo, mut merged, host) = fixture_report();
        merged.state_dirs.push(".local/share/opencode".to_string());
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 2,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: true,
            result: false,
        })
        .join("\n");
        assert!(
            joined.contains("state dirs:     1 (rw, persisted in the sidecar)"),
            "{joined}"
        );
        assert!(
            joined.contains("  /mysbx-home/.local/share/opencode <-> /synth/repo.mysbx/state/.local/share/opencode  [state]"),
            "{joined}"
        );
        assert!(
            joined.contains("state dir(s) persisted in the sidecar"),
            "{joined}"
        );
    }

    #[test]
    fn without_state_dirs_the_home_line_stays_the_tmpfs_one() {
        let (repo, merged, host) = fixture_report();
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 2,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: true,
            result: false,
        })
        .join("\n");
        assert!(
            joined.contains(&format!(
                "home:           {SANDBOX_HOME} (tmpfs; the host home is not mounted)"
            )),
            "{joined}"
        );
        assert!(!joined.contains("state dirs:"), "{joined}");
    }

    #[test]
    fn the_multiplexer_is_reported_with_its_in_sandbox_socket_and_only_for_the_shell() {
        // cli.md D10/D11: the report is where an operator checks the
        // isolation claim, so the socket path is spelled out — and the
        // `run` form says out loud that the session is NOT started.
        for mux in [
            Multiplexer::Tmux,
            Multiplexer::Workmux,
            Multiplexer::Herdr,
            Multiplexer::Aoe,
            Multiplexer::Orca,
        ] {
            let (repo, mut merged, host) = fixture_report();
            merged.multiplexer = mux;
            let params = Params {
                shell: "/synth/bin/bash",
                tools_path: "/synth/bin",
                bin_sh: None,
                nix_conf: None,
                ca_bundle: None,
                policy_paths: &[],
                mux_entry: Some("/synth/bin/mysbx-mux-entry"),
                waypipe: None,
                workspace: Workspace::Live,
            };
            let report_of = |payload: &Payload| {
                lines(&Report {
                    repo: &repo,
                    sidecar_exists: true,
                    user_config: Path::new("/synth/xdg/mysbx/config.toml"),
                    user_config_exists: true,
                    sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
                    sidecar_config_exists: false,
                    merged: &merged,
                    backend_from_cli: false,
                    user_mount_count: 1,
                    cli_mount_count: 0,
                    host_env: &host,
                    params: &params,
                    bwrap_bin: "bwrap",
                    backend: "bubblewrap",
                    image: None,
                    payload,
                    dry_run: true,
                    result: false,
                })
                .join("\n")
            };

            let shell = report_of(&Payload::Shell);
            // The selected multiplexer is named, and so is the private
            // socket directory it runs on.
            assert!(
                shell.contains(&format!("multiplexer:    {mux} —")),
                "{shell}"
            );
            assert!(
                shell.contains(&format!("private socket dir {MUX_SOCKET_DIR}")),
                "{shell}"
            );
            assert!(
                shell.contains(&format!(
                    "payload:        {mux} session /synth/bin/mysbx-mux-entry"
                )),
                "{shell}"
            );

            let cmd = report_of(&Payload::Command(vec!["ls".into()]));
            assert!(cmd.contains("but not applied"), "{cmd}");
            assert!(cmd.contains("payload:        command ls"), "{cmd}");
            assert!(!cmd.contains("private socket dir"), "{cmd}");
        }

        // With `none` (the shared fixture) the report says so instead
        // of staying silent: which payload an interactive run starts
        // has five possible answers now, so every run states it.
        let plain = render(true, &Payload::Shell).join("\n");
        assert!(
            plain.contains("multiplexer:    none — no session is started"),
            "{plain}"
        );
        assert!(!plain.contains("private socket dir"), "{plain}");
        assert!(
            plain.contains("payload:        shell /synth/bin/bash"),
            "{plain}"
        );
    }

    #[test]
    fn the_display_is_reported_with_its_channel_and_refusals() {
        // cli.md D10: the report is where an operator checks the display
        // channel against the argv — the host-side socket path, the
        // guest display name, the guest binary, and the refusal note
        // when nothing is pinned. `off` is stated like `none` is: which
        // display a run has is a property of every run.
        let (repo, mut merged, host) = fixture_report();
        merged.display = Display::Waypipe;
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: Some(crate::bwrap::Waypipe {
                socket_dir: "/synth/repo.mysbx/waypipe/1234",
                guest_bin: "/synth/bin/waypipe",
            }),
            workspace: Workspace::Live,
        };
        let report_of = |payload: &Payload| {
            lines(&Report {
                repo: &repo,
                sidecar_exists: true,
                user_config: Path::new("/synth/xdg/mysbx/config.toml"),
                user_config_exists: true,
                sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
                sidecar_config_exists: false,
                merged: &merged,
                backend_from_cli: false,
                user_mount_count: 1,
                cli_mount_count: 0,
                host_env: &host,
                params: &params,
                bwrap_bin: "bwrap",
                backend: "bubblewrap",
                image: None,
                payload,
                dry_run: true,
                result: false,
            })
            .join("\n")
        };
        let shell = report_of(&Payload::Shell);
        assert!(
            shell.contains(
                "display:         waypipe — socket /synth/repo.mysbx/waypipe/1234/waypipe.sock"
            ),
            "{shell}"
        );
        assert!(
            shell.contains(&format!(
                "guest display {WAYPIPE_DISPLAY} under {SANDBOX_HOME}"
            )),
            "{shell}"
        );
        assert!(
            shell.contains("  guest bin:    /synth/bin/waypipe"),
            "{shell}"
        );
        // The env-provenance marks apply with the display on: a layer
        // or a forwarded host variable naming XDG_RUNTIME_DIR or
        // WAYLAND_DISPLAY never reaches the payload.
        let mut with_env = merged.clone();
        with_env
            .env
            .insert("XDG_RUNTIME_DIR".into(), "/synth/evil".into());
        let mut forwarded = HostEnv::new();
        forwarded.insert("WAYLAND_DISPLAY".into(), "host-0".into());
        let marked = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: false,
            merged: &with_env,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 0,
            host_env: &forwarded,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: true,
            result: false,
        })
        .join("\n");
        assert!(
            marked.contains("XDG_RUNTIME_DIR=/synth/evil  [config, ignored — set by mysbx]"),
            "{marked}"
        );
        assert!(
            marked.contains("WAYLAND_DISPLAY=host-0  [host, ignored — set by mysbx]"),
            "{marked}"
        );

        // Without the pin the report says the run is refused.
        let mut unrefused = params.clone();
        unrefused.waypipe = None;
        let refused = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: false,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 0,
            host_env: &host,
            params: &unrefused,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: true,
            result: false,
        })
        .join("\n");
        assert!(
            refused.contains("display:         waypipe, refused — no waypipe pinned"),
            "{refused}"
        );

        // And the default: `off` is stated out loud, and the env marks
        // do NOT apply — a configured XDG_RUNTIME_DIR reaches the
        // payload when no display channel overrides it.
        let plain = render(true, &Payload::Shell).join("\n");
        assert!(
            plain.contains("display:         off — the run is headless"),
            "{plain}"
        );
    }

    #[test]
    fn the_report_attributes_cli_additions_to_the_command_line() {
        // cli.md D16/D10: the `--ro`/`--rw` additions are the LAST
        // mounts of the merged list, so the last `cli_mount_count`
        // ones are labeled `command line` — a distinct provenance from
        // both config layers, so the report never claims a grant came
        // from a file the operator never edited.
        let (repo, mut merged, host) = fixture_report();
        merged.mounts.push(Mount {
            path: "/synth/granted".into(),
            dest: None,
            mode: Mode::Ro,
        });
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        let payload = Payload::Shell;
        let out = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: false,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 1,
            host_env: &host,
            params: &params,
            bwrap_bin: "/synth/bin/bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &payload,
            dry_run: true,
            result: false,
        });
        assert!(
            out.contains(&"##   ro /synth/granted -> /synth/granted  [command line]".to_owned()),
            "{out:?}"
        );
        // The layers keep their labels.
        assert!(
            out.contains(&"##   rw /synth/shared -> /synth/shared  [user config]".to_owned()),
            "{out:?}"
        );
        assert!(
            out.contains(&"##   ro /synth/shared/sub -> /inside  [sidecar config]".to_owned()),
            "{out:?}"
        );
    }

    #[test]
    fn a_result_run_names_its_mode_instead_of_claiming_an_exec() {
        // cli.md D17: a `--result` run WAITS for the payload — the
        // `mode:` line must say so. Claiming "executing" would be the
        // same lie as a dry run claiming it: the exit code of a waited
        // run is interpreted, not passed through.
        let (repo, merged, host) = fixture_report();
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Command(vec!["ls".into()]),
            dry_run: false,
            result: true,
        })
        .join("\n");
        assert!(
            joined.contains("mode:           waiting for the payload"),
            "{joined}"
        );
        assert!(!joined.contains("executing"), "{joined}");
    }

    // ---- the clone runs of the workspace model (workspace.md D3/D4) ----------

    #[test]
    fn a_clone_run_report_names_the_workspace_and_marks_downgraded_mounts() {
        // D3/D4 in the report: the workspace line says WHICH tree is
        // bound (the clone, at the repo's own path) and that the host
        // repo is not mounted; every downgraded mount is marked, the
        // implicit bind is the session clone, and the state-dirs of
        // the fixture are OFF in a clone run (D4) instead of claimed
        // as persisted.
        let (repo, mut merged, host) = fixture_report();
        merged.state_dirs.push(".local/share/opencode".to_string());
        let clone = PathBuf::from("/synth/repo.mysbx/clones/fix-1");
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: crate::bwrap::Workspace::Clone { clone: &clone },
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: true,
            result: false,
        })
        .join("\n");
        assert!(
            joined.contains(
                "workspace:      clone run — /synth/repo.mysbx/clones/fix-1 bound rw at /synth/repo (the host repo is not mounted)",
            ),
            "{joined}"
        );
        // The implicit bind is the session clone at the repo path,
        // not the repo.
        assert!(
            joined.contains(
                "  rw /synth/repo.mysbx/clones/fix-1 -> /synth/repo  [session clone, implicit]"
            ),
            "{joined}"
        );
        assert!(!joined.contains("[repo, implicit]"), "{joined}");
        // The rw mount of the fixture is downgraded AND marked (D4 +
        // cli.md D10), never silently narrowed.
        assert!(
            joined.contains(
                "  ro /synth/shared -> /synth/shared  [user config, downgraded — clone run]"
            ),
            "{joined}"
        );
        // The ro mount keeps its mode and its plain provenance.
        assert!(
            joined.contains("  ro /synth/shared/sub -> /inside  [sidecar config]"),
            "{joined}"
        );
        // The state dirs are OFF (D4), said out loud — and the home
        // line does not claim the sidecar persistence a live run
        // would show.
        assert!(
            joined.contains(
                "state dirs:     off in a clone run (1 declared, not handled — workspace.md D4)"
            ),
            "{joined}"
        );
        assert!(
            joined.contains(&format!(
                "home:           {SANDBOX_HOME} (tmpfs; the host home is not mounted)"
            )),
            "{joined}"
        );
        assert!(!joined.contains("persisted in the sidecar"), "{joined}");
    }

    #[test]
    fn a_live_run_report_stays_the_live_one() {
        // D1: the live default is untouched — the workspace line says
        // live, the repo bind keeps its [repo, implicit] line, and an
        // rw mount is NOT marked downgraded.
        let (repo, merged, host) = fixture_report();
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            bin_sh: None,
            nix_conf: None,
            ca_bundle: None,
            policy_paths: &[],
            mux_entry: None,
            waypipe: None,
            workspace: Workspace::Live,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            backend_from_cli: false,
            user_mount_count: 1,
            cli_mount_count: 0,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            backend: "bubblewrap",
            image: None,
            payload: &Payload::Shell,
            dry_run: true,
            result: false,
        })
        .join("\n");
        assert!(
            joined.contains("workspace:      live — the repo itself, bound rw (the default)"),
            "{joined}"
        );
        assert!(
            joined.contains("rw /synth/repo -> /synth/repo  [repo, implicit]"),
            "{joined}"
        );
        assert!(
            joined.contains("rw /synth/shared -> /synth/shared  [user config]"),
            "{joined}"
        );
        assert!(!joined.contains("downgraded"), "{joined}");
    }
}
