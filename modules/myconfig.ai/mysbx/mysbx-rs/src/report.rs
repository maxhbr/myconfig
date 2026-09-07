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

use crate::bwrap::{HostEnv, Params, Payload, SANDBOX_HOME, WORKMUX_SOCKET_DIR};
use crate::config::Mode;
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
    /// How many of `merged.mounts` came from the user layer. The merge
    /// puts the user mounts first, in declaration order, then the
    /// accepted sidecar mounts — so this single number attributes every
    /// mount to its layer without re-deriving the merge.
    pub user_mount_count: usize,
    /// The forwarded host variables (already filtered to the ones that
    /// were actually set).
    pub host_env: &'a HostEnv,
    pub params: &'a Params<'a>,
    /// The backend binary that would be executed (`MYSBX_BWRAP` after
    /// its fallback).
    pub bwrap_bin: &'a str,
    pub payload: &'a Payload,
    /// Whether this run stops before `exec` — said out loud, so the
    /// report never claims a command ran that did not.
    pub dry_run: bool,
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
        "backend:        {}",
        r.merged.backend.as_deref().unwrap_or("(none)")
    ));
    p(format!(
        "network:        {}",
        if r.merged.network {
            "shared (--share-net)"
        } else {
            "denied (--unshare-all, no --share-net)"
        }
    ));

    // Mounts, in argv order: the implicit repo bind first (config.md
    // D13), then the configured mounts in declaration order.
    p(format!(
        "mounts:         {} (in declaration order)",
        r.merged.mounts.len() + 1
    ));
    p(format!(
        "  rw {} -> {}  [repo, implicit]",
        r.repo.root.display(),
        r.repo.root.display()
    ));
    for (i, m) in r.merged.mounts.iter().enumerate() {
        let layer = if i < r.user_mount_count {
            "user config"
        } else {
            "sidecar config"
        };
        p(format!(
            "  {} {} -> {}  [{layer}]",
            match m.mode {
                Mode::Ro => "ro",
                Mode::Rw => "rw",
            },
            m.path,
            m.dest.as_deref().unwrap_or(&m.path),
        ));
    }

    // The sandbox's own home (config.md D14): a tmpfs, not a bind, so it
    // is not part of the mount list above — but an operator reading the
    // report must see where `$HOME` points and that it is not the host's.
    // State dirs (D15) bind subdirectories of it to sidecar-backed
    // stores, so with any declared entry the parenthetical says so
    // instead of implying an all-ephemeral home.
    p(if r.merged.state_dirs.is_empty() {
        format!("home:           {SANDBOX_HOME} (tmpfs; the host home is not mounted)")
    } else {
        format!(
            "home:           {SANDBOX_HOME} (tmpfs + {} state dir(s) persisted in the sidecar; the host home is not mounted)",
            r.merged.state_dirs.len(),
        )
    });

    // State dirs (config.md D15), in declaration order: what the sandbox
    // persists across runs and where the backing store lives. They are
    // implicit binds like the repo, so they belong with the mount
    // listing's provenance, not buried in prose.
    if !r.merged.state_dirs.is_empty() {
        p(format!(
            "state dirs:     {} (rw, persisted in the sidecar)",
            r.merged.state_dirs.len()
        ));
        // The in-sandbox path in full (`/mysbx-home/<entry>`), not the
        // bare entry: the report is read against the argv, where the
        // dest is spelled out, and `/<entry>` would read like a path at
        // the sandbox root.
        for entry in &r.merged.state_dirs {
            p(format!(
                "  {SANDBOX_HOME}/{entry} <-> {}  [state]",
                r.repo.sidecar.join("state").join(entry).display()
            ));
        }
    }

    // Environment. Values are shown verbatim; see the module docs for
    // why they are not redacted.
    p(format!(
        "env:            {} forwarded from the host, {} from [env] (values shown verbatim — they may be secrets)",
        r.host_env.len(),
        r.merged.env.len(),
    ));
    for (k, v) in r.host_env {
        // Both sections are printed in argv order (host first, `[env]`
        // second), and the argv builder lets the later `--setenv` win —
        // so a host variable that `[env]` also sets never reaches the
        // payload. Say so, instead of listing the same name twice with
        // two values and no hint which one applies.
        if infrastructure(k) {
            p(format!("  {k}={v}  [host, ignored — set by mysbx]"));
        } else if r.merged.env.contains_key(k) {
            p(format!("  {k}={v}  [host, overridden by [env]]"));
        } else {
            p(format!("  {k}={v}  [host]"));
        }
    }
    for (k, v) in &r.merged.env {
        if infrastructure(k) {
            // `HOME` and `PATH` are set after `[env]` (config.md D14), so
            // a layer that names them never reaches the payload.
            p(format!("  {k}={v}  [config, ignored — set by mysbx]"));
        } else {
            p(format!("  {k}={v}  [config]"));
        }
    }
    p(format!("  HOME={SANDBOX_HOME}  [sandbox home]"));
    p(format!("  PATH={}  [tools]", r.params.tools_path));

    p(format!("bwrap:          {}", r.bwrap_bin));
    p(format!("shell:          {}", r.params.shell));
    p(format!("tools PATH:     {}", r.params.tools_path));
    // Review-2 item 3: the host's /etc/nix/nix.conf is never mounted
    // (it may hold access-tokens); what the sandbox sees is the
    // generated, credential-free file the wrapper pins — or nothing.
    // Both states belong in the report: "nix reads no configuration"
    // is as much a property of the run as which shell it starts.
    p(format!(
        "nix.conf:       {}",
        r.params
            .nix_conf
            .unwrap_or("(none — nix uses its defaults)")
    ));
    // workmux (config.md D16 / cli.md D11), only when a layer asked
    // for it: what replaces the shell, where its tmux socket lives,
    // and — for the `run` form — that this run keeps the plain
    // payload after all. Saying the socket path out loud is the point:
    // "the socket is inside the sandbox" is the isolation claim, and
    // the report is where an operator checks it against the argv.
    if r.merged.workmux {
        match r.payload {
            Payload::Shell => {
                p(format!(
                    "workmux:        enabled — tmux socket {WORKMUX_SOCKET_DIR}/socket (inside the sandbox home tmpfs; no host tmux server is reachable)"
                ));
                p(format!(
                    "  entry:        {}",
                    r.params.workmux_entry.unwrap_or("(none pinned — the run is refused)")
                ));
            }
            Payload::Command(_) => p(
                "workmux:        enabled, but not applied — it replaces the INTERACTIVE payload only (cli.md D11)"
                    .to_string(),
            ),
        }
    }

    match r.payload {
        Payload::Shell if r.merged.workmux => p(format!(
            "payload:        workmux session {}",
            r.params.workmux_entry.unwrap_or("(none pinned)")
        )),
        Payload::Shell => p(format!("payload:        shell {}", r.params.shell)),
        // Space-joined for readability only; the exact, unambiguous
        // argument vector is what `--dry-run` prints.
        Payload::Command(args) => p(format!("payload:        command {}", args.join(" "))),
    }
    p(if r.dry_run {
        "mode:           dry run — the argv follows, nothing is executed".into()
    } else {
        "mode:           executing".to_string()
    });
    out
}

/// The variables `bwrap_argv` sets last and no layer can override
/// (config.md D14).
fn infrastructure(key: &str) -> bool {
    key == "HOME" || key == "PATH"
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
    use crate::config::Mount;
    use std::collections::BTreeMap;
    use std::path::PathBuf;

    fn fixture_report() -> (Repo, Merged, HostEnv) {
        let repo = Repo {
            root: PathBuf::from("/synth/repo"),
            sidecar: PathBuf::from("/synth/repo.mysbx"),
            git_dirs: Vec::new(),
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
            workmux: false,
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
            nix_conf: None,
            policy_paths: &[],
            workmux_entry: None,
        };
        lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: false,
            merged: &merged,
            user_mount_count: 1,
            host_env: &host,
            params: &params,
            bwrap_bin: "/synth/bin/bwrap",
            payload,
            dry_run,
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
            nix_conf: None,
            policy_paths: &[],
            workmux_entry: None,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            user_mount_count: 1,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            payload: &Payload::Shell,
            dry_run: true,
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
            nix_conf: None,
            policy_paths: &[],
            workmux_entry: None,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            user_mount_count: 1,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            payload: &Payload::Shell,
            dry_run: true,
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
            nix_conf: None,
            policy_paths: &[],
            workmux_entry: None,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: false,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: false,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            user_mount_count: 2,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            payload: &Payload::Shell,
            dry_run: false,
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
            nix_conf: None,
            policy_paths: &[],
            workmux_entry: None,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            user_mount_count: 2,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            payload: &Payload::Shell,
            dry_run: true,
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
            nix_conf: None,
            policy_paths: &[],
            workmux_entry: None,
        };
        let joined = lines(&Report {
            repo: &repo,
            sidecar_exists: true,
            user_config: Path::new("/synth/xdg/mysbx/config.toml"),
            user_config_exists: true,
            sidecar_config: Path::new("/synth/repo.mysbx/config.toml"),
            sidecar_config_exists: true,
            merged: &merged,
            user_mount_count: 2,
            host_env: &host,
            params: &params,
            bwrap_bin: "bwrap",
            payload: &Payload::Shell,
            dry_run: true,
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
    fn workmux_is_reported_with_its_in_sandbox_socket_and_only_for_the_shell() {
        // cli.md D10/D11: the report is where an operator checks the
        // isolation claim, so the socket path is spelled out — and the
        // `run` form says out loud that the session is NOT started.
        let (repo, mut merged, host) = fixture_report();
        merged.workmux = true;
        let params = Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
            nix_conf: None,
            policy_paths: &[],
            workmux_entry: Some("/synth/bin/mysbx-workmux-entry"),
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
                user_mount_count: 1,
                host_env: &host,
                params: &params,
                bwrap_bin: "bwrap",
                payload,
                dry_run: true,
            })
            .join("\n")
        };

        let shell = report_of(&Payload::Shell);
        assert!(
            shell.contains(&format!("tmux socket {WORKMUX_SOCKET_DIR}/socket")),
            "{shell}"
        );
        assert!(
            shell.contains("payload:        workmux session /synth/bin/mysbx-workmux-entry"),
            "{shell}"
        );

        let cmd = report_of(&Payload::Command(vec!["ls".into()]));
        assert!(cmd.contains("enabled, but not applied"), "{cmd}");
        assert!(cmd.contains("payload:        command ls"), "{cmd}");
        assert!(!cmd.contains("tmux socket"), "{cmd}");

        // Nothing at all without the key (the shared fixture has
        // `workmux: false`), so the report of every other run is
        // unchanged.
        assert!(!render(true, &Payload::Shell).join("\n").contains("workmux"));
    }
}
