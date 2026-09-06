// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Golden argv tests (docs/TODOs/mvp-4-bwrap-argv.md).
//!
//! Every fixture under `tests/assets/argv/*.txt` is the *expected* argv of
//! [`bwrap_argv`] for one case, one argument per line, compared byte for
//! byte. Item 5 (`--dry-run`) prints exactly this format, so the assets
//! double as its acceptance fixtures.
//!
//! All paths are synthetic (`/synth/...`): [`bwrap_argv`] is pure — it
//! canonicalizes nothing and checks no existence (the merge already did
//! D8) — so the fixtures need no tempdirs and are machine-independent.
//! Only the base binds are fixed absolute host paths (`/nix/store`, …),
//! and those are identical on every machine.

use mysbx::bwrap::{bwrap_argv, HostEnv, Params, Payload, SANDBOX_HOME};
use mysbx::config::{Mode, Mount};
use mysbx::merge::Merged;
use mysbx::repo::Repo;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// A synthetic repo (paths need not exist — the function is pure).
fn synth_repo() -> Repo {
    Repo {
        root: PathBuf::from("/synth/repo"),
        sidecar: PathBuf::from("/synth/repo.mysbx"),
    }
}

/// A synthetic `Merged`: bubblewrap, the given network switch, nothing else.
fn base(network: bool) -> Merged {
    Merged {
        backend: Some("bubblewrap".into()),
        network,
        mounts: Vec::new(),
        env: BTreeMap::new(),
    }
}

fn make_mount(path: &str, dest: Option<&str>, mode: Mode) -> Mount {
    Mount {
        path: path.to_string(),
        dest: dest.map(|d| d.to_string()),
        mode,
    }
}

fn params() -> Params<'static> {
    Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
    }
}

/// The argv, one argument per line (the intended `--dry-run` format of
/// item 5, which prints this very vector).
fn rendered(argv: &[String]) -> String {
    let mut out = String::new();
    for arg in argv {
        out.push_str(arg);
        out.push('\n');
    }
    out
}

fn golden(rel: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/assets/argv")
        .join(rel);
    match std::fs::read_to_string(&path) {
        Ok(text) => text,
        Err(e) => panic!("cannot read golden asset {}: {e}", path.display()),
    }
}

fn golden_path(rel: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/assets/argv")
        .join(rel)
}

/// Compare one case against its golden file, byte for byte; on mismatch
/// show the first differing line so the fix is obvious. With
/// `MYSBX_BLESS_GOLDEN=1` the actual argv is written to the asset instead
/// — the documented way to regenerate the goldens after a deliberate
/// argv change.
fn assert_golden(rel: &str, argv: &[String]) {
    let actual = rendered(argv);
    if std::env::var_os("MYSBX_BLESS_GOLDEN").is_some() {
        std::fs::write(golden_path(rel), &actual)
            .unwrap_or_else(|e| panic!("cannot bless {rel}: {e}"));
        return;
    }
    let expected = golden(rel);
    let actual = rendered(argv);
    if expected != actual {
        let exp: Vec<&str> = expected.lines().collect();
        let act: Vec<&str> = actual.lines().collect();
        for i in 0..exp.len().max(act.len()) {
            let e = exp.get(i).copied().unwrap_or("<eof>");
            let a = act.get(i).copied().unwrap_or("<eof>");
            if e != a {
                panic!(
                    "{rel}: golden mismatch at line {} (1-based):\n  expected: {e:?}\n  actual:   {a:?}",
                    i + 1
                );
            }
        }
        unreachable!();
    }
}

/// A fixed `HostEnv`; the builder is pure, so the host variables come in
/// as a parameter and the tests pin whatever subset they pass.
fn host_env(pairs: &[(&str, &str)]) -> HostEnv {
    pairs
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

/// Positions of all `--setenv` keys in the argv, in order.
fn setenv_keys(argv: &[String]) -> Vec<&str> {
    argv.iter()
        .zip(argv.iter().skip(1))
        .filter(|(a, _)| a.as_str() == "--setenv")
        .map(|(_, b)| b.as_str())
        .collect()
}

/// Sources of all mount binds (`--ro-bind` / `--bind` first arguments).
fn bind_sources(argv: &[String]) -> Vec<&str> {
    argv.windows(2)
        .filter(|w| w[0] == "--ro-bind" || w[0] == "--bind")
        .map(|w| w[1].as_str())
        .collect()
}

/// All `(source, dest)` bind pairs — the remap direction matters for
/// the absence tests: binding something ONTO a protected path is the
/// dangerous direction a source-only view cannot see.
fn bind_pairs(argv: &[String]) -> Vec<(&str, &str)> {
    argv.windows(3)
        .filter(|w| w[0] == "--ro-bind" || w[0] == "--bind")
        .map(|w| (w[1].as_str(), w[2].as_str()))
        .collect()
}

// ---- the golden cases (spec list, in order) --------------------------------

#[test]
fn golden_minimal_config() {
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_golden("minimal.txt", &argv);
}

#[test]
fn golden_one_ro_mount() {
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data/refs", None, Mode::Ro));
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_golden("ro-mount.txt", &argv);
}

#[test]
fn golden_one_rw_mount() {
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data/cache", None, Mode::Rw));
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_golden("rw-mount.txt", &argv);
}

#[test]
fn golden_mount_with_explicit_dest() {
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/data/configs",
        Some("/inside/x"),
        Mode::Ro,
    ));
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_golden("explicit-dest.txt", &argv);
}

#[test]
fn golden_network_false() {
    let argv = bwrap_argv(
        &base(false),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_golden("network-false.txt", &argv);
    // A deny sandbox is recognisable by the ABSENCE of the share (spec
    // section 2); --unshare-all is present either way.
    assert!(!argv.contains(&"--share-net".to_string()));
    assert!(argv.contains(&"--unshare-all".to_string()));
}

#[test]
fn golden_env_entry() {
    // An [env] entry wins over a host-forwarded variable of the same name
    // by being applied later; the golden file pins the exact order.
    let mut cfg = base(true);
    cfg.env.insert("EDITOR".into(), "repo-nvim".into());
    cfg.env.insert("PROJECT".into(), "demo".into());
    let host = host_env(&[
        ("TERM", "xterm-256color"),
        ("COLORTERM", "truecolor"),
        ("LANG", "C.UTF-8"),
        ("EDITOR", "host-nvim"),
    ]);
    let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host, &params());
    assert_golden("env-entry.txt", &argv);
}

#[test]
fn golden_sidecar_narrows_user_config() {
    // docs/TODOs/mvp-3-layer-merge.md end state, hand-built (a `Merged`
    // merge.rs would have produced and the argv builder alone sees): the
    // user config grants two mounts; the sidecar adds a ro mount below a
    // grant and introduces a sidecar-only variable. User mounts first,
    // accepted sidecar mounts after.
    let cfg = Merged {
        backend: Some("bubblewrap".into()),
        network: true,
        mounts: vec![
            make_mount("/synth/data/refs", None, Mode::Ro), // user
            make_mount("/synth/data/cache", None, Mode::Rw), // user
            make_mount("/synth/data/refs/docs", None, Mode::Ro), // sidecar, below a grant
        ],
        env: BTreeMap::from([
            ("EDITOR".to_string(), "user-nvim".to_string()), // user layer
            ("PROJECT".to_string(), "demo".to_string()),     // sidecar may introduce
        ]),
    };
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_golden("sidecar-narrowed.txt", &argv);
}

#[test]
fn golden_interactive_payload() {
    // The same shape as `golden_minimal_config`, pinned separately so a
    // change to the payload section cannot hide behind the rest.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_golden("interactive-shell.txt", &argv);
}

#[test]
fn golden_command_payload_with_flag_looking_args() {
    // cli.md D4: everything after `--` is verbatim and never parsed —
    // even flag-looking arguments stay payload content.
    let payload = Payload::Command(vec![
        "agent".into(),
        "-x".into(),
        "--help".into(),
        "--unknown-flag".into(),
        "value with spaces".into(),
    ]);
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &payload,
        &host_env(&[]),
        &params(),
    );
    assert_golden("command-with-flags.txt", &argv);
    // And the payload really is everything after the single `--`.
    let dd = argv.iter().position(|x| x == "--").unwrap();
    assert_eq!(&argv[dd + 1..], &argv[argv.len() - 5..]);
}

// ---- structural assertions beyond the goldens ------------------------------

#[test]
fn no_run_no_host_home_beyond_declared_mounts() {
    // The "no" rows of the base table (docs/plan.md): `/run` is never
    // mounted, the HOST home directory is never reachable except through
    // a mount that declares it, and there is no automatic
    // `OPENAI_API_KEY` forward. The `$HOME` row is a different claim:
    // `$HOME` inside the sandbox is a tmpfs (config.md D14), backed by
    // nothing on the host — see `sandbox_home_is_a_tmpfs_outside_home`.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data/refs", None, Mode::Ro));
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );

    // Every bind source is the repo or a declared mount — nothing else
    // from the host is reachable.
    let allowed: BTreeMap<&str, ()> = cfg
        .mounts
        .iter()
        .map(|m| (m.path.as_str(), ()))
        .chain([
            ("/synth/repo", ()),
            // the fixed base binds of the base table
            ("/nix/store", ()),
            ("/usr/bin", ()),
            ("/etc/localtime", ()),
        ])
        .collect();
    for src in bind_sources(&argv) {
        assert!(
            allowed.contains_key(src),
            "bind source {src} is not a declared mount or the repo"
        );
    }

    let text = argv.join(" ");
    // /run must not appear as a bind source or DEST (structural): the
    // `--ro-bind X /run…` remap direction is the dangerous one and a
    // substring check alone cannot tell them apart.
    for src in bind_sources(&argv) {
        assert!(!src.starts_with("/run"), "no /run bind source: {src}");
    }
    for (src, dest) in bind_pairs(&argv) {
        assert!(
            !dest.starts_with("/run"),
            "bind {src} -> {dest} touches /run"
        );
    }
    assert!(!text.contains("/home/"), "no host home path: {text}");
    assert!(!text.contains("$HOME"), "no literal $HOME: {text}");
    assert!(
        !text.contains("OPENAI_API_KEY"),
        "no auto key forward: {text}"
    );
    assert!(!argv.iter().any(|x| x == "~/tmp"), "no ~/tmp bind");
}

#[test]
fn share_net_iff_network_true() {
    let repo = synth_repo();
    let p = params();
    for (network, expect_share) in [(true, true), (false, false)] {
        let argv = bwrap_argv(&base(network), &repo, &Payload::Shell, &host_env(&[]), &p);
        assert_eq!(argv.contains(&"--share-net".to_string()), expect_share);
        assert!(argv.contains(&"--unshare-all".to_string()));
    }
}

#[test]
fn mount_order_is_preserved() {
    // A later rw bind nested inside an earlier ro bind is a real pattern
    // (the base table relies on it): the argv must keep declaration order.
    let cfg = Merged {
        backend: Some("bubblewrap".into()),
        network: true,
        mounts: vec![
            make_mount("/synth/data/outer", None, Mode::Ro),
            make_mount("/synth/data/outer/nested", None, Mode::Rw),
        ],
        env: BTreeMap::new(),
    };
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    let outer = argv
        .iter()
        .position(|x| x.as_str() == "/synth/data/outer")
        .unwrap();
    let nested = argv
        .iter()
        .position(|x| x.as_str() == "/synth/data/outer/nested")
        .unwrap();
    assert!(
        outer < nested,
        "ro outer bind must precede the nested rw bind"
    );
    assert_eq!(argv[outer - 1], "--ro-bind", "outer mount is ro");
    assert_eq!(argv[nested - 1], "--bind", "nested mount is rw");
}

#[test]
fn forward_only_set_host_variables() {
    // The builder never reads the environment (it is pure); it sets
    // exactly what the caller passed in `host_env` — plus [env] and PATH.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[("TERM", "xterm"), ("LC_ALL", "C")]),
        &params(),
    );
    assert_eq!(
        setenv_keys(&argv),
        // HostEnv is a BTreeMap: keys come in sorted order (deterministic,
        // which is what a golden argv needs). The infrastructure pair
        // `HOME`, `PATH` always last (config.md D14).
        vec!["LC_ALL", "TERM", "HOME", "PATH"],
    );
}

#[test]
fn env_precedence_host_then_config_then_path() {
    // Order of the --setenv section: host-forwarded, then [env] (wins by
    // being set later), then PATH last of all.
    let mut cfg = base(true);
    cfg.env.insert("TERM".into(), "cfg-wins".into());
    cfg.env.insert("PROJECT".into(), "x".into());
    let host = host_env(&[("TERM", "host-val"), ("EDITOR", "host-nvim")]);
    let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host, &params());
    assert_eq!(
        setenv_keys(&argv),
        // host keys sorted (BTreeMap), then [env], then HOME and PATH
        vec!["EDITOR", "TERM", "PROJECT", "TERM", "HOME", "PATH"],
    );
    // The later TERM value really is the [env] one.
    let vals: Vec<&str> = argv
        .iter()
        .zip(argv.iter().skip(1))
        .filter(|(a, _)| a.as_str() == "TERM")
        .map(|(_, b)| b.as_str())
        .collect();
    assert_eq!(vals, vec!["host-val", "cfg-wins"]);
}

#[test]
fn tmpfs_tmp_is_not_host_backed() {
    // The /tmp row of the base table: tmpfs, not a bind of any host dir.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    let t = argv.iter().position(|x| x.as_str() == "--tmpfs").unwrap();
    assert_eq!(argv[t + 1], "/tmp");
    for w in argv.windows(3) {
        assert!(
            !((w[0] == "--bind" || w[0] == "--ro-bind") && w[2] == "/tmp"),
            "/tmp must not be bind-backed: {:?}",
            argv
        );
    }
}

#[test]
fn sandbox_home_is_a_tmpfs_outside_home() {
    // The `$HOME` row of the base table (config.md D14): the sandbox has
    // a home of its own so `cd ~` works, it is a tmpfs (nothing of the
    // host behind it), it is not below `/home`, and `HOME` names it.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    let tmpfs: Vec<&str> = argv
        .windows(2)
        .filter(|w| w[0] == "--tmpfs")
        .map(|w| w[1].as_str())
        .collect();
    assert_eq!(tmpfs, vec!["/tmp", SANDBOX_HOME]);
    assert!(!SANDBOX_HOME.starts_with("/home"), "{SANDBOX_HOME}");
    for (src, dest) in bind_pairs(&argv) {
        assert_ne!(
            dest, SANDBOX_HOME,
            "the sandbox home is bind-backed by {src}"
        );
    }
    let i = argv.iter().position(|x| x == "HOME").unwrap();
    assert_eq!(&argv[i - 1..i + 2], &["--setenv", "HOME", SANDBOX_HOME]);
}

#[test]
fn config_env_cannot_repoint_home_or_path() {
    // config.md D14: both infrastructure variables are set after `[env]`,
    // so the effective values are mysbx's, whatever a layer says.
    let mut cfg = base(true);
    cfg.env.insert("HOME".into(), "/synth/evil-home".into());
    cfg.env.insert("PATH".into(), "/synth/evil-bin".into());
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_eq!(
        setenv_keys(&argv),
        vec!["HOME", "PATH", "HOME", "PATH"],
        "[env] entries first, the infrastructure pair last"
    );
    let n = argv.len();
    // … and the last pair really carries mysbx's values.
    assert_eq!(
        &argv[n - 10..n - 4],
        &[
            "--setenv",
            "HOME",
            SANDBOX_HOME,
            "--setenv",
            "PATH",
            "/synth/bin"
        ]
    );
}

// ---- hidden mounts (review-1 finding 3) -------------------------------------

#[test]
#[should_panic(expected = "would hide earlier mount")]
fn parent_after_child_hides_the_child_and_panics() {
    // The review scenario: ro `.ssh` FIRST, rw `/home/u` SECOND. The
    // later wide bind replaces the subtree the narrow one landed on,
    // leaving `.ssh` writable — refuse to build such an argv at all.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u/.ssh", None, Mode::Ro));
    cfg.mounts.push(make_mount("/synth/u", None, Mode::Rw));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params());
}

#[test]
fn child_after_parent_is_the_safe_direction_and_stays_allowed() {
    // Wide rw FIRST, narrow ro SECOND: the narrow bind lands ON TOP of
    // the wide one — the documented narrowing-by-shadowing pattern.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u", None, Mode::Rw));
    cfg.mounts.push(make_mount("/synth/u/.ssh", None, Mode::Ro));
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    );
    assert_eq!(
        argv.windows(3)
            .filter(|w| w[0] == "--ro-bind" && w[2] == "/synth/u/.ssh")
            .count(),
        1,
        "the narrow ro bind is present"
    );
}

#[test]
fn equal_dest_rebind_does_not_panic() {
    // Same dest twice: shadowing re-bind, policed by the merge's grant
    // checks, not a hidden mount.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/a", Some("/synth/dst"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/b", Some("/synth/dst"), Mode::Rw));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params());
}

#[test]
#[should_panic(expected = "would hide earlier mount")]
fn hiding_is_judged_on_dest_not_source() {
    // Sources are unrelated; the DESTS make the later mount hide the
    // earlier one. `dest` defaults to the source path when absent.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/elsewhere", Some("/synth/u/.ssh"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/other", Some("/synth/u"), Mode::Rw));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params());
}

#[test]
fn sibling_dests_and_untouched_rebinds_stay_allowed() {
    // No ancestor relation, no hiding; equal dest via defaulting also
    // fine (already covered), and a later mount BELOW an earlier one in
    // a different subtree is plain independent configuration.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u", None, Mode::Rw));
    cfg.mounts.push(make_mount("/synth/v", None, Mode::Rw));
    cfg.mounts
        .push(make_mount("/synth/w", Some("/synth/u/w"), Mode::Ro));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params());
}

#[test]
#[should_panic(expected = "would hide the repo working tree")]
fn mount_covering_the_repo_is_refused() {
    // The repo bind (always rw, D13) is implicit and comes FIRST; a
    // configured mount whose dest covers it would replace what --chdir
    // lands in — equal dest included, the repo is not configuration.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/data", Some("/synth"), Mode::Rw));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params());
}

#[test]
#[should_panic(expected = "would hide the repo working tree")]
fn mount_exactly_on_the_repo_is_refused() {
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/synth/repo"), Mode::Rw));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params());
}

#[test]
fn mount_below_the_repo_stays_allowed() {
    // Narrowing BELOW the repo root is the legitimate pattern.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/synth/repo/sub"), Mode::Ro));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params());
}

#[test]
#[should_panic(expected = "would hide earlier mount")]
fn hidden_mounts_are_judged_after_dest_normalization() {
    // `..` components must be collapsed BEFORE the ancestor comparison,
    // else `/synth/u/../u` style dests slip past the hiding guard just
    // like they slipped past the protected-dest guard before finding 1.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u/.ssh", None, Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/other", Some("/synth/u/../u"), Mode::Rw));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params());
}
