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
        git_dirs: Vec::new(),
    }
}

/// A synthetic repo whose `.git` file points at `git_dirs` (worktree
/// layout, review-2 item 1). The builder is pure, so the paths need
/// not exist; the approval list is what gates the bind.
fn worktree_repo(git_dirs: &[&str]) -> Repo {
    Repo {
        root: PathBuf::from("/synth/repo"),
        sidecar: PathBuf::from("/synth/repo.mysbx"),
        git_dirs: git_dirs.iter().map(PathBuf::from).collect(),
    }
}

/// A synthetic `Merged`: bubblewrap, the given network switch, nothing else.
fn base(network: bool) -> Merged {
    Merged {
        backend: Some("bubblewrap".into()),
        network,
        mounts: Vec::new(),
        env: BTreeMap::new(),
        git_dirs: Vec::new(),
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
    ).unwrap();
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
    ).unwrap();
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
    ).unwrap();
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
    ).unwrap();
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
    ).unwrap();
    assert_golden("network-false.txt", &argv);
    // A deny sandbox is recognisable by the ABSENCE of the share (spec
    // section 2); --unshare-all is present either way.
    assert!(!argv.contains(&"--share-net".to_string()));
    assert!(argv.contains(&"--unshare-all".to_string()));
    // Review-1 finding 5: a denied network binds NO resolver paths
    // either — the resolver set belongs to the share, not the base.
    assert!(
        !argv.iter().any(|a| a.contains("resolv") || a.contains("/etc/hosts")),
        "no resolver binds when the network is denied: {argv:?}"
    );
    assert!(!argv.contains(&"/run/systemd/resolve".to_string()));
}

#[test]
fn network_share_binds_the_resolver_set() {
    // Review-1 finding 5: sharing the namespace alone gives no DNS/TLS.
    // The five resolver paths are bound ro, --ro-bind-try (they are
    // setup-dependent), right after --share-net and BEFORE the base
    // binds — so the golden files show them at a fixed position.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    ).unwrap();
    assert_eq!(argv[0], "--clearenv");
    assert_eq!(argv[1], "--unshare-all");
    assert_eq!(argv[2], "--share-net");
    // The resolver block starts immediately: nothing ro-bind-try may
    // sneak in ahead of it (so the slice below is unambiguous).
    assert_eq!(argv[3], "--ro-bind-try");
    let resolver_binds: Vec<&str> = argv
        .windows(3)
        .filter(|w| w[0] == "--ro-bind-try")
        .map(|w| w[1].as_str())
        .take(5)
        .collect();
    assert_eq!(
        resolver_binds,
        [
            "/etc/hosts",
            "/etc/nsswitch.conf",
            "/etc/resolv.conf",
            "/etc/ssl",
            "/run/systemd/resolve",
        ]
    );
    // … and nothing else BETWEEN the resolver block and the base binds
    // is a ro-bind-try; the base's own two try-binds (`/nix/var/nix`,
    // `/etc/nix/nix.conf`, review-1 finding 6) come later, in base order.
    let after_resolver = &argv[3 + 3 * 5..];
    let base_try: Vec<&str> = after_resolver
        .windows(3)
        .filter(|w| w[0] == "--ro-bind-try")
        .map(|w| w[1].as_str())
        .collect();
    assert_eq!(base_try, ["/nix/var/nix", "/etc/nix/nix.conf"]);
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
    let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host, &params()).unwrap();
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
        git_dirs: Vec::new(),
    };
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    ).unwrap();
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
    ).unwrap();
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
    ).unwrap();
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
    ).unwrap();

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
        let argv = bwrap_argv(&base(network), &repo, &Payload::Shell, &host_env(&[]), &p).unwrap();
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
        git_dirs: Vec::new(),
    };
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    ).unwrap();
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
    ).unwrap();
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
    let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host, &params()).unwrap();
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
    ).unwrap();
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
    ).unwrap();
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
    ).unwrap();
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
fn parent_after_child_hides_the_child_is_refused() {
    const EXPECTED: &str = "would hide earlier mount";
    // The review scenario: ro `.ssh` FIRST, rw `/home/u` SECOND. The
    // later wide bind replaces the subtree the narrow one landed on,
    // leaving `.ssh` writable — refuse to build such an argv at all.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u/.ssh", None, Mode::Ro));
    cfg.mounts.push(make_mount("/synth/u", None, Mode::Rw));

    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string().contains(EXPECTED)
            && matches!(err, mysbx::bwrap::Error::HiddenMount { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn child_after_a_writable_parent_is_refused() {
    // Wide rw FIRST, narrow ro SECOND used to be the documented
    // narrowing-by-shadowing pattern. Review-2 item 2 refuses it: the
    // payload can plant `/synth/u/.ssh -> /etc` in the rw tree, and
    // the NEXT run's dest resolves through that symlink, landing the
    // bind wherever the symlink points.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u", None, Mode::Rw));
    cfg.mounts.push(make_mount("/synth/u/.ssh", None, Mode::Ro));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn child_after_a_read_only_parent_stays_allowed() {
    // The same shape with a RO parent stays allowed: its content is
    // host state the sandbox cannot rewrite, so no symlink can be
    // planted there from inside (review-2 item 2 scopes the refusal to
    // writable binds).
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u", None, Mode::Ro));
    cfg.mounts.push(make_mount("/synth/u/.ssh", None, Mode::Ro));
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    ).unwrap();
    assert_eq!(
        argv.windows(3)
            .filter(|w| w[0] == "--ro-bind" && w[2] == "/synth/u/.ssh")
            .count(),
        1,
        "the narrow ro bind is present"
    );
}

#[test]
fn equal_dest_rebind_stays_allowed() {
    // Same dest twice: shadowing re-bind, policed by the merge's grant
    // checks, not a hidden mount.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/a", Some("/synth/dst"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/b", Some("/synth/dst"), Mode::Rw));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params()).unwrap();
}

#[test]
fn hiding_is_judged_on_dest_not_source() {
    const EXPECTED: &str = "would hide earlier mount";
    // Sources are unrelated; the DESTS make the later mount hide the
    // earlier one. `dest` defaults to the source path when absent.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/elsewhere", Some("/synth/u/.ssh"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/other", Some("/synth/u"), Mode::Rw));

    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string().contains(EXPECTED)
            && matches!(err, mysbx::bwrap::Error::HiddenMount { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn sibling_dests_and_untouched_rebinds_stay_allowed() {
    // No ancestor relation, no hiding; equal dest via defaulting also
    // fine (already covered), and a later mount BELOW an earlier one in
    // a different subtree is plain independent configuration.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u", None, Mode::Ro));
    cfg.mounts.push(make_mount("/synth/v", None, Mode::Rw));
    cfg.mounts
        .push(make_mount("/synth/w", Some("/synth/u/w"), Mode::Ro));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params()).unwrap();
}

#[test]
fn mount_covering_the_repo_is_refused() {
    const EXPECTED: &str = "would hide the repo working tree";
    // The repo bind (always rw, D13) is implicit and comes FIRST; a
    // configured mount whose dest covers it would replace what --chdir
    // lands in — equal dest included, the repo is not configuration.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/data", Some("/synth"), Mode::Rw));

    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string().contains(EXPECTED)
            && matches!(err, mysbx::bwrap::Error::HiddenMount { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn mount_exactly_on_the_repo_is_refused() {
    const EXPECTED: &str = "would hide the repo working tree";
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/synth/repo"), Mode::Rw));

    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string().contains(EXPECTED)
            && matches!(err, mysbx::bwrap::Error::HiddenMount { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn mount_below_the_repo_is_refused() {
    // Review-2 item 2: the repo is writable and its content decides
    // how a dest below it resolves (`<repo>/jump -> /`), so mounts may
    // not land inside the work tree at all.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/synth/repo/sub"), Mode::Ro));
    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn hidden_mounts_are_judged_after_dest_normalization() {
    const EXPECTED: &str = "would hide earlier mount";
    // `..` components must be collapsed BEFORE the ancestor comparison,
    // else `/synth/u/../u` style dests slip past the hiding guard just
    // like they slipped past the protected-dest guard before finding 1.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u/.ssh", None, Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/other", Some("/synth/u/../u"), Mode::Rw));

    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string().contains(EXPECTED)
            && matches!(err, mysbx::bwrap::Error::HiddenMount { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn worktree_git_dirs_are_bound_rw() {
    // Review-1 finding 4: the `.git` FILE's targets must be bound rw at
    // their real host paths, common dir BEFORE the per-worktree gitdir,
    // right after the repo bind and before any configured mount — but
    // only when approved (review-2 item 1): the approval list of the
    // trusted layers must cover them.
    let repo = worktree_repo(&["/synth/main/.git/worktrees/wt"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    let argv = bwrap_argv(
        &cfg,
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    ).unwrap();
    // Positions: repo bind first, then the approved git dir, both rw.
    let repo_bind = pos_pair(&argv, "--bind", "/synth/repo");
    let gitdir = pos_pair(&argv, "--bind", "/synth/main/.git/worktrees/wt");
    assert!(repo_bind < gitdir, "repo bind comes first");
    // ro binds must not have been used for git metadata.
    assert_eq!(pos_ro_bind(&argv, "/synth/main/.git/worktrees/wt"), None);
}

#[test]
fn plain_repo_adds_no_git_binds() {
    // git_dirs empty: the argv has exactly one --bind for the repo and
    // no other.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    ).unwrap();
    assert_eq!(
        argv.windows(3)
            .filter(|w| w[0] == "--bind")
            .count(),
        1,
        "only the repo bind"
    );
}

/// Index of the `flag src` pair, panicking when absent. Also asserts
/// the third element matches `src`, so a `--bind src other-dest` pair
/// does not satisfy a source-position query.
fn pos_pair(argv: &[String], flag: &str, src: &str) -> usize {
    argv.windows(3)
        .position(|w| w[0] == flag && w[1] == src && w[2] == src)
        .unwrap_or_else(|| panic!("missing {flag} {src}"))
}

/// Index of the `--ro-bind src` pair, `None` when absent.
fn pos_ro_bind(argv: &[String], src: &str) -> Option<usize> {
    argv.windows(2).position(|w| w[0] == "--ro-bind" && w[1] == src)
}

#[test]
fn mount_covering_a_git_dir_is_refused() {
    const EXPECTED: &str = "would hide a git metadata directory";
    // The git dir binds are implicit infrastructure like the repo: a
    // configured mount covering one would silently break `git status`
    // inside the sandbox. The git dir is approved (review-2 item 1),
    // so the refusal really is the hiding check.
    let repo = worktree_repo(&["/synth/main/.git/worktrees/wt"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    cfg.mounts
        .push(make_mount("/synth/data", Some("/synth/main"), Mode::Rw));

    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string().contains(EXPECTED)
            && matches!(err, mysbx::bwrap::Error::HiddenMount { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn nix_store_db_and_config_are_bound() {
    // Review-1 finding 6: `nix` is on the sandbox PATH, so
    // /nix/var/nix (store database, daemon socket) and the host's
    // nix.conf must be bound ro — the two binds the base of
    // fns/bubblewrap-app.nix makes -- right after /nix/store, in base
    // order, --ro-bind-try (both are absent on non-NixOS hosts).
    let argv = bwrap_argv(
        &base(false),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    ).unwrap();
    let store = pos_pair(&argv, "--ro-bind", "/nix/store");
    let var_nix = pos_pair(&argv, "--ro-bind-try", "/nix/var/nix");
    let nix_conf = pos_pair(&argv, "--ro-bind-try", "/etc/nix/nix.conf");
    let usr_bin = pos_pair(&argv, "--ro-bind", "/usr/bin");
    assert!(store < var_nix, "/nix/store before /nix/var/nix");
    assert!(var_nix < nix_conf && nix_conf < usr_bin, "base order kept");
}

#[test]
fn mount_dest_onto_nix_var_is_refused() {
    const EXPECTED: &str = "would shadow or overwrite the protected";
    // The base's nix paths are protected like every base bind.
    let mut cfg = base(false);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/nix/var/nix"), Mode::Rw));

    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string().contains(EXPECTED)
            && matches!(err, mysbx::bwrap::Error::ProtectedDest { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn mount_dest_below_nix_var_is_refused() {
    const EXPECTED: &str = "would shadow or overwrite the protected";
    // A descendant of the store-database bind replaces part of it.
    let mut cfg = base(false);
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/nix/var/nix/daemon-socket"),
        Mode::Rw,
    ));

    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string().contains(EXPECTED)
            && matches!(err, mysbx::bwrap::Error::ProtectedDest { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn mount_dest_elsewhere_in_nix_stays_allowed() {
    // `/nix/var` is NOT protected as a whole and neither is `/etc/nix`:
    // only the two bound paths are; a dest beside them is ordinary.
    let mut cfg = base(false);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/nix/var/other"), Mode::Rw));
    cfg.mounts
        .push(make_mount("/synth/data", Some("/etc/nix/other.conf"), Mode::Ro));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params()).unwrap();
}

// ---- git metadata approval (review-2 item 1) -------------------------------

#[test]
fn unapproved_git_dir_is_refused() {
    // The core adversarial case: a repo-writable `.git` FILE points at
    // an arbitrary host directory. Without approval, the builder must
    // refuse the bind — the pointer is untrusted content (config.md D3)
    // and grants nothing.
    let repo = worktree_repo(&["/synth/target"]);
    let err = bwrap_argv(
        &base(true),
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        err.to_string().contains("not approved")
            && matches!(err, mysbx::bwrap::Error::GitDirNotApproved { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn approved_git_dir_below_the_entry_is_bound() {
    // Approval is by containment: an entry covers everything at or
    // below it, like a mount grant.
    let repo = worktree_repo(&["/synth/main/.git/worktrees/wt"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    let argv = bwrap_argv(
        &cfg,
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert!(pos_pair(&argv, "--bind", "/synth/main/.git/worktrees/wt") > 0);
}

#[test]
fn exact_approval_is_enough() {
    let repo = worktree_repo(&["/synth/target"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/target")];
    let argv = bwrap_argv(
        &cfg,
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert!(pos_pair(&argv, "--bind", "/synth/target") > 0);
}

#[test]
fn sibling_approval_does_not_cover() {
    // Component-boundary containment: `/synth/targets` does not approve
    // `/synth/target`.
    let repo = worktree_repo(&["/synth/target"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/targets")];
    let err = bwrap_argv(
        &cfg,
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::GitDirNotApproved { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn root_git_dir_is_refused_even_if_listed() {
    // `gitdir: /` must never bind — even if a hostile or sloppy config
    // lists `/` in `git-dirs`, and even though repo resolution refuses
    // it earlier in the real pipeline. The builder is the last line.
    let repo = worktree_repo(&["/"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/")];
    let err = bwrap_argv(
        &cfg,
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::GitDirProtected { protected: "/", .. }),
        "wrong error: {err}"
    );
}

#[test]
fn protected_related_git_dir_is_refused_even_if_listed() {
    // A gitdir related to a protected sandbox path (an ancestor of
    // /nix/store here) would shadow base infrastructure exactly like
    // a bad mount dest — refused regardless of the approval list.
    let repo = worktree_repo(&["/nix"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/nix")];
    let err = bwrap_argv(
        &cfg,
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(
            err,
            mysbx::bwrap::Error::GitDirProtected {
                protected: "/nix/store",
                ..
            }
        ),
        "wrong error: {err}"
    );
}

// ---- dests below writable binds (review-2 item 2) --------------------------

#[test]
fn the_jump_symlink_scenario_is_refused_lexically() {
    // The review's scenario, verbatim: `<repo>/jump -> /` makes a dest
    // of `<repo>/jump/tmp` resolve to the protected `/tmp` when
    // bubblewrap applies the bind. The builder cannot see the symlink
    // (it is pure, and a host-side canonicalize would model the wrong
    // tree and race anyway), so it refuses the whole class: any dest
    // below the repo.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/synth/repo/jump/tmp"),
        Mode::Rw,
    ));
    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn dest_below_a_git_dir_is_refused() {
    // Git metadata is bound rw too, and `git` writes there: same
    // symlink-planting surface as the work tree.
    let repo = worktree_repo(&["/synth/main/.git/worktrees/wt"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/synth/main/.git/worktrees/wt/hooks"),
        Mode::Ro,
    ));
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_read_only_reexposure_of_repo_content_is_writable_too() {
    // `ro` stops writes THROUGH the bind, not writes to the same host
    // inode through the repo bind next door: a ro mount of a path
    // inside the repo carries payload-planted symlinks just like the
    // repo, so a dest below it is refused as well.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/repo/tools", Some("/opt/tools"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/data", Some("/opt/tools/x"), Mode::Ro));
    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_read_only_reexposure_of_a_writable_mount_is_writable_too() {
    // Same one layer out: the rw mount makes its SOURCE writable, and
    // a later ro mount of a path inside that source inherits the
    // property.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/work", Some("/work"), Mode::Rw));
    cfg.mounts
        .push(make_mount("/synth/work/sub", Some("/opt/sub"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/data", Some("/opt/sub/x"), Mode::Ro));
    let err = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_read_only_mount_of_ordinary_host_state_stays_a_usable_parent() {
    // The carve-out that keeps ro nesting usable: a granted host path
    // outside every writable tree cannot be rewritten from inside the
    // sandbox, so a dest below it is allowed.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/etc", Some("/opt/etc"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/data", Some("/opt/etc/x"), Mode::Ro));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params()).unwrap();
}

#[test]
fn equal_dest_on_a_writable_mount_stays_allowed() {
    // An equal dest resolves the path itself, not a component INSIDE
    // the writable content, so re-binding stays the documented
    // shadowing pattern.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/a", Some("/synth/dst"), Mode::Rw));
    cfg.mounts
        .push(make_mount("/synth/b", Some("/synth/dst"), Mode::Ro));
    bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params()).unwrap();
}

#[test]
fn dest_below_the_sandbox_home_stays_allowed() {
    // The tmpfs home is created empty by bubblewrap in this very run:
    // nothing can have planted a symlink in it, so seeding dotfiles
    // below it (config.md D14) stays the intended pattern.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/dotfiles/gitconfig",
        Some("/mysbx-home/.gitconfig"),
        Mode::Ro,
    ));
    let argv =
        bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &params()).unwrap();
    assert!(argv.contains(&"/mysbx-home/.gitconfig".to_string()));
}
