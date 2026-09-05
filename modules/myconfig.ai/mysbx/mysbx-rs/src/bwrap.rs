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
//! ("Watch out" in the spec).

use crate::config::Mode;
use crate::merge::Merged;
use crate::repo::Repo;
use std::collections::BTreeMap;
use std::path::Path;

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
///    `/etc/localtime` ro, tmpfs `/tmp`
/// 4. the repo itself, read-write, at its real host path
///    (docs/design/config.md D13)
/// 5. the configured mounts, in declaration order, `--ro-bind` / `--bind`,
///    each `dest` defaulting to its source path (mount order is argv
///    order; a later rw bind nested inside an earlier ro bind is a real
///    pattern the MVP must preserve)
/// 6. environment via `--setenv`, in this precedence: host-forwarded
///    variables first, then `cfg.env` (which wins by being set later),
///    then `PATH` last
/// 7. `--chdir` into the repo root
/// 8. `--` and the payload, verbatim
///
/// Deliberately absent (see the base table's "no" rows): `/run`, `~/tmp`,
/// a host-backed `/tmp/<name>`, and any automatic `OPENAI_API_KEY` —
/// under `mysbx` a key is an ordinary user-config `[env]` entry
/// (docs/design/config.md D6). Nothing is forwarded implicitly: only the
/// variables the caller put in `host_env` reach the sandbox.
pub fn bwrap_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
    host_env: &HostEnv,
    params: &Params<'_>,
) -> Vec<String> {
    let root = repo.root.to_string_lossy().into_owned();
    let mut argv: Vec<String> = vec!["--clearenv".into(), "--unshare-all".into()];
    if cfg.network {
        argv.push("--share-net".into());
    }

    // 3. the base binds (docs/plan.md "The base" table, fixed absolute
    // host paths — machine-independent).
    argv.extend(base_binds());

    // 4. the repo, rw, at its real host path (D13).
    bind(&mut argv, false, &root, None);

    // 5. configured mounts, in declaration order; dest defaults to the
    // canonicalized source path. A `dest` may never remap a mount ONTO a
    // protected path: bubblewrap applies binds in order with
    // later-mounts-win, so a dest of `/tmp`, `/`, `/proc` … would
    // overwrite a base bind and reopen exactly the hole the base table
    // closes (host-backed `/tmp`, a hidden `/proc`). The merge (D7/D8)
    // validates grants; this validates destinations, because the base
    // list lives here.
    for m in &cfg.mounts {
        let dest = m.dest.as_deref().unwrap_or(&m.path);
        for protected in PROTECTED_DESTS {
            // `/` itself must match EXACTLY (`starts_with("/")` would
            // match every absolute path); the others match at-or-below.
            let hits = if *protected == "/" {
                dest == "/"
            } else {
                Path::new(dest).starts_with(protected)
            };
            if hits {
                // A mount that does not redirect (dest == source) can
                // never hit this: its source is an ordinary granted host
                // path, not a base path — the merge would have had to
                // grant `/proc` itself for that. So any hit here means a
                // redirect onto a protected path.
                panic!(
                    "mount dest {dest} would overwrite the protected sandbox \n\
                 path {protected} (base table of docs/plan.md); refusing \n\
                 to build the argv"
                );
            }
        }
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

    argv
}

/// The fixed base binds of the MVP (docs/plan.md, base table). Every row
/// with decision "yes" appears exactly once, in the order the existing
/// `fns/bubblewrap-app.nix` base binds them (agents shell out to
/// arbitrary store paths → `/nix/store` first; `/usr/bin/env` shebangs →
/// `/usr/bin`; timezones → `/etc/localtime`; a fresh tmpfs `/tmp`, NOT
/// the host-backed one).
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
    ]
}

/// Sandbox paths a mount `dest` may never overwrite. The roots the base
/// binds create (`/nix/store`, `/usr/bin`, `/proc`, `/dev`,
/// `/etc/localtime`, `/tmp`) plus `/run` (deliberately absent, so also
/// protected) — and `/` itself, which would shadow every one of them at
/// once. The repo root is deliberately NOT here: it is a base bind of
/// its own (section 4) and a mount legitimately points at or below it.
static PROTECTED_DESTS: &[&str] = &[
    "/",
    "/nix/store",
    "/usr/bin",
    "/proc",
    "/dev",
    "/etc/localtime",
    "/tmp",
    "/run",
];

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
        }
    }

    fn merged() -> Merged {
        Merged {
            backend: Some("bubblewrap".into()),
            network: true,
            mounts: Vec::new(),
            env: BTreeMap::new(),
        }
    }

    fn params() -> Params<'static> {
        Params {
            shell: "/synth/bin/bash",
            tools_path: "/synth/bin",
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
        let n = argv.len();
        assert!(n >= 2);
        assert_eq!(argv[n - 2], "--");
        assert_eq!(argv[n - 1], "/synth/bin/bash");
    }

    #[test]
    fn command_payload_is_verbatim() {
        let (repo, cfg, p) = shell_repo_defaults();
        let payload = Payload::Command(vec!["ls".into(), "-x".into(), "--help".into()]);
        let argv = bwrap_argv(&cfg, &repo, &payload, &HostEnv::new(), &p);
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
        assert!(argv.contains(&"--share-net".to_string()));

        let mut deny = cfg;
        deny.network = false;
        let argv = bwrap_argv(&deny, &repo, &Payload::Shell, &HostEnv::new(), &p);
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
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &host, &p);
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
        // … and `PATH` last of all env; nothing follows it but the
        // `--chdir` and payload sections.
        assert_eq!(argv[setenvs[4]], "--setenv");
        assert_eq!(argv[setenvs[4] + 1], "PATH");
        assert_eq!(argv[setenvs[4] + 2], "/synth/bin");
        assert_eq!(
            argv.len() - setenvs[4] - 3,
            4, // --chdir /synth/repo -- /synth/bin/bash
            "nothing after PATH but --chdir, -- and the payload"
        );
    }

    #[test]
    fn no_run_no_home_no_openai() {
        let (repo, cfg, p) = shell_repo_defaults();
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
        let joined = argv.join(" ");
        assert!(!joined.contains("/run"), "no /run bind");
        // No `$HOME` bind, no host home path, no `~/tmp` — and no automatic
        // secret forwards (the OPENAI row of the base table).
        assert!(!joined.contains("$HOME"));
        assert!(!joined.contains("/home/"), "no host home path anywhere");
        assert!(!joined.contains("OPENAI_API_KEY"));
        assert!(!argv.contains(&"~/tmp".to_string()));
    }

    #[test]
    #[should_panic(expected = "would overwrite the protected sandbox")]
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
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
    }

    #[test]
    #[should_panic(expected = "would overwrite the protected sandbox")]
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
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
    }

    #[test]
    #[should_panic(expected = "would overwrite the protected sandbox")]
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
        bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
    }

    #[test]
    fn mount_dest_inside_repo_is_fine() {
        // The legitimate remap: a granted host path re-exposed under a
        // (renamed) path inside the repo, or the repo itself as dest.
        let (repo, cfg, p) = shell_repo_defaults();
        let mut cfg = cfg;
        cfg.mounts = vec![
            Mount {
                path: "/synth/data".into(),
                dest: Some("/synth/repo/.data".into()),
                mode: Mode::Ro,
            },
            Mount {
                path: "/synth/repo/tools".into(),
                dest: Some("/synth/repo/tools".into()),
                mode: Mode::Ro,
            },
        ];
        let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &HostEnv::new(), &p);
        let pairs: Vec<_> = argv
            .windows(3)
            .filter(|w| w[0] == "--ro-bind")
            .map(|w| (w[1].as_str(), w[2].as_str()))
            .collect();
        assert!(pairs.contains(&("/synth/data", "/synth/repo/.data")));
        assert!(pairs.contains(&("/synth/repo/tools", "/synth/repo/tools")));
    }
}
