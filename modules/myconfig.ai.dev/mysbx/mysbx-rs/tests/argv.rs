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
use mysbx::config::{Mode, Mount, Multiplexer};
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
        worktrees: None,
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
        worktrees: None,
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
        state_dirs: Vec::new(),
        multiplexer: Multiplexer::None,
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
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &[],
        mux_entry: None,
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
    )
    .unwrap();
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
    )
    .unwrap();
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
    )
    .unwrap();
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
    )
    .unwrap();
    assert_golden("explicit-dest.txt", &argv);
}

#[test]
fn golden_state_dirs() {
    // Docs/design/config.md D15: the state binds come after the repo
    // (and its git dirs) and before every configured mount — implicit
    // infrastructure, order-stable in declaration order. The source is
    // synthesized from the sidecar (`<sidecar>/state/<entry>`), the
    // dest below the sandbox home; the mount from the golden
    // explicit-dest case follows after them.
    let mut cfg = base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.state_dirs.push(".local/state/opencode".to_string());
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
    )
    .unwrap();
    assert_golden("state-dirs.txt", &argv);
}

#[test]
fn nested_state_dirs_are_refused() {
    // D15: a nested pair is an ambiguous layout (the inner bind would
    // land inside the outer entry's backing directory), refused with
    // the dedicated error.
    let mut cfg = base(true);
    cfg.state_dirs.push(".local/share".to_string());
    cfg.state_dirs.push(".local/share/opencode".to_string());
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::StateDirNesting { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn sibling_state_dirs_are_fine() {
    // Disjoint entries (the common pattern: one per tool) stay
    // declarable side by side.
    let mut cfg = base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.state_dirs.push(".local/share/pi".to_string());
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert!(argv.windows(3).any(|w| {
        w[0] == "--bind"
            && w[1] == "/synth/repo.mysbx/state/.local/share/opencode"
            && w[2] == "/mysbx-home/.local/share/opencode"
    }));
    assert!(argv.windows(3).any(|w| {
        w[0] == "--bind"
            && w[1] == "/synth/repo.mysbx/state/.local/share/pi"
            && w[2] == "/mysbx-home/.local/share/pi"
    }));
}

#[test]
fn a_mount_covering_a_state_dir_is_refused() {
    // State binds are implicit infrastructure like the repo and the
    // git dirs: a configured mount whose dest covers one replaces the
    // subtree wholesale — the state directory would silently stop
    // being what a layer declared.
    let mut cfg = base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/mysbx-home/.local/share/opencode"),
        Mode::Rw,
    ));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        err.to_string().contains("would hide a state directory"),
        "wrong error: {err}"
    );
}

#[test]
fn a_mount_dest_below_a_state_dir_is_refused() {
    // Review-2 item 2 for the state binds: the payload can write the
    // state tree (it is an rw bind), so a dest below it can be reached
    // through a payload-planted symlink — refused like the same shape
    // against the repo.
    let mut cfg = base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/mysbx-home/.local/share/opencode/jump"),
        Mode::Ro,
    ));
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
fn an_rw_mount_above_a_state_dir_is_refused() {
    // D15: the state tree decides where the next run's state binds
    // come from. An `rw` mount of the sidecar's `state/` directory
    // (which holds no policy file, so the policy-file guard does not
    // catch it) lets the payload swap a component for a symlink, and
    // the next run would bind its target rw into the sandbox home.
    let mut cfg = base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.mounts.push(make_mount(
        "/synth/repo.mysbx/state",
        Some("/statetree"),
        Mode::Rw,
    ));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::StateTreeWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn an_rw_mount_of_the_state_dir_itself_stays_allowed() {
    // Only a writable PARENT is the problem: the backing store itself
    // is already rw inside the sandbox by construction, and the
    // payload cannot rewrite its own parent entry.
    let mut cfg = base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.mounts.push(make_mount(
        "/synth/repo.mysbx/state/.local/share/opencode",
        Some("/elsewhere"),
        Mode::Rw,
    ));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
}

#[test]
fn a_ro_mount_of_the_sidecar_state_tree_stays_allowed() {
    // Reviewing the state tree from inside the sandbox (an `ro` mount
    // of the sidecar's `state/` directory, like the ro sidecar mount
    // of review-3 item 3) writes nothing in place and stays declarable.
    let mut cfg = base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.mounts.push(make_mount(
        "/synth/repo.mysbx/state",
        Some("/review/state"),
        Mode::Ro,
    ));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
}

#[test]
fn golden_network_false() {
    let argv = bwrap_argv(
        &base(false),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert_golden("network-false.txt", &argv);
    // A deny sandbox is recognisable by the ABSENCE of the share (spec
    // section 2); --unshare-all is present either way.
    assert!(!argv.contains(&"--share-net".to_string()));
    assert!(argv.contains(&"--unshare-all".to_string()));
    // Review-1 finding 5: a denied network binds NO resolver paths
    // either — the resolver set belongs to the share, not the base.
    assert!(
        !argv.iter().any(|a| a.contains("resolv")
            || a.contains("/etc/hosts")
            || a.contains("/etc/ssl")
            || a.contains("/etc/static")),
        "no resolver binds when the network is denied: {argv:?}"
    );
    assert!(!argv.contains(&"/run/systemd/resolve".to_string()));
}

#[test]
fn the_pinned_ca_bundle_sets_the_tls_env_after_config_env() {
    // bd myconfig-938, step 2: a pinned `MYSBX_CA_BUNDLE` sets
    // `SSL_CERT_FILE`/`GIT_SSL_CAINFO`/`NIX_SSL_CERT_FILE` AFTER the
    // `[env]` block — infrastructure like `HOME` and `PATH`, so a
    // layer that names them never reaches the payload — and BEFORE
    // `--chdir`: section order is semantic, so the position is part
    // of the pinned behavior.
    let params = Params {
        ca_bundle: Some("/nix/store/aaaa-nss-cacert-bundle/etc/ssl/certs/ca-bundle.crt"),
        ..params()
    };
    let mut cfg = base(true);
    cfg.env
        .insert("SSL_CERT_FILE".into(), "/attacker-controlled".into());
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    let bundle = "/nix/store/aaaa-nss-cacert-bundle/etc/ssl/certs/ca-bundle.crt";
    for key in ["SSL_CERT_FILE", "GIT_SSL_CAINFO", "NIX_SSL_CERT_FILE"] {
        let at = argv
            .windows(3)
            .filter(|w| w[0] == "--setenv" && w[1] == key)
            .last()
            .unwrap_or_else(|| panic!("{key} is not set: {argv:?}"));
        assert_eq!(at[2], bundle, "{key} must name the pinned bundle");
    }
    // The later --setenv wins in bubblewrap, so the infrastructure
    // entries must come AFTER the [env] one — the pinned value wins.
    let set_env_positions: Vec<usize> = argv
        .iter()
        .enumerate()
        .filter(|(_, a)| *a == "--setenv")
        .map(|(i, _)| i)
        .collect();
    let cfg_ssl = argv
        .windows(3)
        .position(|w| {
            w[0] == "--setenv" && w[1] == "SSL_CERT_FILE" && w[2] == "/attacker-controlled"
        })
        .expect("the [env] entry must still be in the argv (it shows up in --dry-run)");
    let pinned_ssl = argv
        .windows(3)
        .position(|w| w[0] == "--setenv" && w[1] == "SSL_CERT_FILE" && w[2] == bundle)
        .expect("the pinned entry must be in the argv");
    assert!(cfg_ssl < pinned_ssl, "the pin must win: {argv:?}");
    // And both come before the chdir: environment is section 6.
    assert!(pinned_ssl < argv.iter().position(|a| a == "--chdir").unwrap());
    let _ = set_env_positions;
}

#[test]
fn without_a_pinned_ca_bundle_no_tls_env_is_set() {
    // Unset means "no env variables", never "invent a path": pointing
    // `SSL_CERT_FILE` at a nonexistent file would break every tool that
    // honors it, worse than the resolver binds alone.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    for key in ["SSL_CERT_FILE", "GIT_SSL_CAINFO", "NIX_SSL_CERT_FILE"] {
        assert!(
            !argv.contains(&key.to_string()),
            "{key} must not be set without a pin: {argv:?}"
        );
    }
}

#[test]
fn network_share_binds_the_resolver_set() {
    // Review-1 finding 5: sharing the namespace alone gives no DNS/TLS.
    // The six resolver paths are bound ro, --ro-bind-try (they are
    // setup-dependent), right after --share-net and BEFORE the base
    // binds — so the golden files show them at a fixed position.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
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
        .take(6)
        .collect();
    assert_eq!(
        resolver_binds,
        [
            "/etc/hosts",
            "/etc/nsswitch.conf",
            "/etc/resolv.conf",
            "/etc/ssl",
            "/etc/static",
            "/run/systemd/resolve",
        ]
    );
    // … and the only try-bind after the resolver block is the nix
    // daemon socket, which rides with the network switch (review-2
    // item 3). No sanitized nix.conf is pinned in these tests, so
    // nothing else follows.
    let after_resolver = &argv[3 + 3 * 6..];
    let base_try: Vec<&str> = after_resolver
        .windows(3)
        .filter(|w| w[0] == "--ro-bind-try")
        .map(|w| w[1].as_str())
        .collect();
    assert_eq!(base_try, ["/nix/var/nix"]);
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
fn golden_ripgrep_config_path_activation() {
    // Review-3 item 6: the file mount alone is inert — Home Manager
    // activates `~/.config/ripgrep/ripgreprc` through
    // `RIPGREP_CONFIG_PATH`, which `--clearenv` kills (it is not in
    // the forwarding allowlist). The generated user layer (default.nix
    // `baselineEnv`) therefore carries the variable as an ordinary
    // `[env]` entry pointing at the IN-SANDBOX path the mount created.
    // This pins what that produces: the mount at
    // /mysbx-home/.config/ripgrep and the setenv, in section order.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/home/u/.config/ripgrep",
        Some("/mysbx-home/.config/ripgrep"),
        Mode::Ro,
    ));
    cfg.env.insert(
        "RIPGREP_CONFIG_PATH".into(),
        "/mysbx-home/.config/ripgrep/ripgreprc".into(),
    );
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert_golden("ripgrep-config-path.txt", &argv);
}

#[test]
fn golden_ca_bundle_pin() {
    // bd myconfig-938, step 2: the pinned CA bundle becomes three
    // infrastructure setenvs after `HOME`/`PATH`, before `--chdir`.
    // The golden file pins the whole argv so the position — and the
    // absence of any other change — is part of the contract.
    let params = Params {
        ca_bundle: Some("/nix/store/aaaa-nss-cacert/etc/ssl/certs/ca-bundle.crt"),
        ..params()
    };
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    assert_golden("ca-bundle-pin.txt", &argv);
}

#[test]
fn golden_git_external_diff_activation() {
    // bd myconfig-kvo: on a difftastic host the generated user layer
    // (default.nix `baselineEnv`) carries `GIT_EXTERNAL_DIFF` pinned at
    // the mysbx-git-default-diff wrapper, because the mounted
    // `~/.config/git` sets `diff.external` and `GIT_EXTERNAL_DIFF` is
    // the override that restores the default unified diff. This pins
    // what that produces: the mount at /mysbx-home/.config/git and the
    // setenv, in section order.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/home/u/.config/git",
        Some("/mysbx-home/.config/git"),
        Mode::Ro,
    ));
    cfg.env.insert(
        "GIT_EXTERNAL_DIFF".into(),
        "/nix/store/0000mysbx-git-default-diff/bin/mysbx-git-default-diff".into(),
    );
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert_golden("git-external-diff.txt", &argv);
}

#[test]
fn golden_both_layers_contribute_mounts() {
    // docs/TODOs/mvp-3-layer-merge.md end state, hand-built (a `Merged`
    // merge.rs would have produced and the argv builder alone sees): the
    // user config declares two mounts; the sidecar adds a third (below
    // one of them) and introduces a sidecar-only variable. User mounts
    // first, sidecar mounts after (config.md D7).
    let cfg = Merged {
        backend: Some("bubblewrap".into()),
        network: true,
        mounts: vec![
            make_mount("/synth/data/refs", None, Mode::Ro), // user
            make_mount("/synth/data/cache", None, Mode::Rw), // user
            make_mount("/synth/data/refs/docs", None, Mode::Ro), // sidecar
        ],
        env: BTreeMap::from([
            ("EDITOR".to_string(), "user-nvim".to_string()), // user layer
            ("PROJECT".to_string(), "demo".to_string()),     // sidecar may introduce
        ]),
        git_dirs: Vec::new(),
        state_dirs: Vec::new(),
        multiplexer: Multiplexer::None,
    };
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert_golden("two-layer-mounts.txt", &argv);
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
    )
    .unwrap();
    assert_golden("interactive-shell.txt", &argv);
}

/// The synthetic entry of one multiplexer, and the golden it produces.
/// One golden per variant: the payload line is the only thing that
/// differs, and pinning each one separately makes an accidental swap
/// (herdr's entry started for `aoe`, say) a visible diff.
fn mux_case(mux: Multiplexer) -> (&'static str, &'static str) {
    match mux {
        Multiplexer::Tmux => ("/synth/bin/mysbx-tmux-entry", "mux-tmux-shell.txt"),
        Multiplexer::Workmux => ("/synth/bin/mysbx-workmux-entry", "mux-workmux-shell.txt"),
        Multiplexer::Herdr => ("/synth/bin/mysbx-herdr-entry", "mux-herdr-shell.txt"),
        Multiplexer::Aoe => ("/synth/bin/mysbx-aoe-entry", "mux-aoe-shell.txt"),
        Multiplexer::None => panic!("`none` starts no session and has no entry"),
    }
}

#[test]
fn golden_multiplexer_interactive_sessions() {
    // docs/design/config.md D17: an INTERACTIVE run with a multiplexer
    // selected swaps the shell for THAT multiplexer's pinned entry and
    // exports `TMUX_TMPDIR` after HOME/PATH — the argv is the
    // auditable form of "the socket lives inside the sandbox". The
    // isolation is identical for every variant, which is the point of
    // running the same assertions over all four.
    for mux in [
        Multiplexer::Tmux,
        Multiplexer::Workmux,
        Multiplexer::Herdr,
        Multiplexer::Aoe,
    ] {
        let (entry, golden) = mux_case(mux);
        let mut cfg = base(true);
        cfg.multiplexer = mux;
        let mut p = params();
        p.mux_entry = Some(entry);
        let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &p).unwrap();
        assert_golden(golden, &argv);
        // The payload is that multiplexer's entry, never the shell.
        assert_eq!(argv[argv.len() - 1], entry);

        // The socket directory is below the sandbox home and no bind
        // puts anything there — the regression guard for "never
        // host-shared", for every multiplexer.
        let keys = setenv_keys(&argv);
        assert_eq!(keys, vec!["HOME", "PATH", "TMUX_TMPDIR"], "{mux}");
        let i = argv.iter().position(|a| a == "TMUX_TMPDIR").unwrap();
        assert_eq!(argv[i + 1], format!("{SANDBOX_HOME}/.mysbx-tmux"));
        for (src, dest) in bind_pairs(&argv) {
            assert!(
                !dest.starts_with(&argv[i + 1]),
                "bind into the socket dir: {src} -> {dest}"
            );
            // The host's tmux socket locations: `$TMUX_TMPDIR` defaults
            // to /tmp (a tmpfs here) and tmux servers of the desktop
            // session live under /run — neither is bound.
            assert!(
                !src.starts_with("/tmp/") && src != "/run",
                "host tmux location bound: {src}"
            );
        }
        assert!(!bind_sources(&argv).contains(&"/tmp"));
    }
}

#[test]
fn the_run_form_is_byte_identical_for_every_multiplexer() {
    // cli.md D11: the integration is interactive-only, so a one-shot
    // `run` argv must not change at all, whichever multiplexer a layer
    // selected.
    let payload = Payload::Command(vec!["ls".into(), "-x".into()]);
    let without = bwrap_argv(
        &base(true),
        &synth_repo(),
        &payload,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    for mux in [
        Multiplexer::Tmux,
        Multiplexer::Workmux,
        Multiplexer::Herdr,
        Multiplexer::Aoe,
    ] {
        let (entry, _) = mux_case(mux);
        let mut p = params();
        p.mux_entry = Some(entry);
        let mut on = base(true);
        on.multiplexer = mux;
        let with = bwrap_argv(&on, &synth_repo(), &payload, &host_env(&[]), &p).unwrap();
        assert_eq!(rendered(&with), rendered(&without), "{mux}");
    }
}

#[test]
fn multiplexer_none_keeps_the_interactive_argv_unchanged() {
    // The other half of the byte-compat contract: a merged value of
    // `none` — the key absent in both layers, or a sidecar that
    // switched a user-config choice off again — gives exactly the
    // pre-existing interactive argv, even with an entry pinned.
    let mut cfg = base(true);
    cfg.multiplexer = Multiplexer::None;
    let mut p = params();
    p.mux_entry = Some("/synth/bin/mysbx-workmux-entry");
    let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &p).unwrap();
    assert_golden("interactive-shell.txt", &argv);
    // Nothing of the pin leaks into the argv, and no socket variable
    // is exported.
    assert!(!argv.iter().any(|a| a.contains("mysbx-workmux-entry")));
    assert!(!argv.iter().any(|a| a == "TMUX_TMPDIR"));
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
    )
    .unwrap();
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
    // a mount that declares it, and the BUILDER forwards nothing on its
    // own — no variable appears that the caller did not hand in via
    // `host_env` (`OPENAI_API_KEY` here stands in for the credential
    // block; the CLI's collection step does forward it when the host
    // sets it, bd myconfig-20j, but this test passes an empty host_env).
    // The `$HOME` row is a different claim:
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
    )
    .unwrap();

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
        "the builder must not invent forwards: {text}"
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
    // Declaration order between UNRELATED mounts must be kept in the
    // argv. (This fixture used to nest an rw bind inside an ro bind;
    // review-3 item 1 turns that pattern into a refusal — see
    // `a_ro_parent_containing_an_rw_mount_is_not_a_safe_parent` — so
    // the order is pinned on disjoint paths instead.)
    let cfg = Merged {
        backend: Some("bubblewrap".into()),
        network: true,
        mounts: vec![
            make_mount("/synth/data/outer", None, Mode::Ro),
            make_mount("/synth/other", None, Mode::Rw),
        ],
        env: BTreeMap::new(),
        git_dirs: Vec::new(),
        state_dirs: Vec::new(),
        multiplexer: Multiplexer::None,
    };
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    let outer = argv
        .iter()
        .position(|x| x.as_str() == "/synth/data/outer")
        .unwrap();
    let nested = argv
        .iter()
        .position(|x| x.as_str() == "/synth/other")
        .unwrap();
    assert!(
        outer < nested,
        "the earlier declared mount must be bound first"
    );
    assert_eq!(argv[outer - 1], "--ro-bind", "outer mount is ro");
    assert_eq!(argv[nested - 1], "--bind", "later mount is rw");
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
    )
    .unwrap();
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
    )
    .unwrap();
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
    )
    .unwrap();
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
    )
    .unwrap();
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

    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
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
    )
    .unwrap();
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
    // Same dest twice: a shadowing re-bind where the later one simply
    // wins, not a hidden mount.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/a", Some("/synth/dst"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/b", Some("/synth/dst"), Mode::Rw));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
}

#[test]
fn hiding_is_judged_on_dest_not_source() {
    const EXPECTED: &str = "would hide earlier mount";
    // Sources are unrelated; the DESTS make the later mount hide the
    // earlier one. `dest` defaults to the source path when absent.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/elsewhere",
        Some("/synth/u/.ssh"),
        Mode::Ro,
    ));
    cfg.mounts
        .push(make_mount("/synth/other", Some("/synth/u"), Mode::Rw));

    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
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
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
}

#[test]
fn mount_covering_the_repo_is_refused() {
    const EXPECTED: &str = "would hide the repo working tree";
    // The repo bind (always rw, D13) is implicit and comes FIRST; a
    // configured mount whose dest covers it would replace what --chdir
    // lands in — equal dest included, the repo is not configuration.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/synth"), Mode::Rw));

    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
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

    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
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
fn hidden_mounts_are_judged_after_dest_normalization() {
    const EXPECTED: &str = "would hide earlier mount";
    // `..` components must be collapsed BEFORE the ancestor comparison,
    // else `/synth/u/../u` style dests slip past the hiding guard just
    // like they slipped past the protected-dest guard before finding 1.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount("/synth/u/.ssh", None, Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/other", Some("/synth/u/../u"), Mode::Rw));

    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
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
    let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params()).unwrap();
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
    )
    .unwrap();
    assert_eq!(
        argv.windows(3).filter(|w| w[0] == "--bind").count(),
        1,
        "only the repo bind"
    );
}

#[test]
fn existing_worktrees_sibling_is_bound_rw() {
    // The workmux `<repo>__worktrees` sibling is implicit
    // infrastructure like the repo bind (D13): rw, at its real host
    // path, after the git metadata binds and before every configured
    // mount — but only when the operator already created it (a run
    // never does), which is why `Repo::worktrees` is an `Option`.
    let mut repo = synth_repo();
    repo.worktrees = Some(PathBuf::from("/synth/repo__worktrees"));
    let argv = bwrap_argv(
        &base(true),
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    let repo_bind = pos_pair(&argv, "--bind", "/synth/repo");
    let worktrees = pos_pair(&argv, "--bind", "/synth/repo__worktrees");
    assert!(repo_bind < worktrees, "the worktrees bind follows the repo");
    assert_eq!(
        pos_ro_bind(&argv, "/synth/repo__worktrees"),
        None,
        "the worktrees bind is rw"
    );
}

#[test]
fn mount_covering_the_worktrees_sibling_is_refused() {
    // The worktrees bind is implicit infrastructure like the repo: a
    // configured mount whose dest covers it would replace the subtree
    // wholesale and silently kill `workmux add`'s workspace.
    let mut repo = synth_repo();
    repo.worktrees = Some(PathBuf::from("/synth/repo__worktrees"));
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/synth/repo__worktrees"),
        Mode::Rw,
    ));

    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        err.to_string()
            .contains("would hide the worktrees directory")
            && matches!(err, mysbx::bwrap::Error::HiddenMount { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn dest_below_the_worktrees_sibling_is_refused() {
    // The worktrees directory is writable host content (the sandbox
    // creates git worktrees in it), so the review-2 item 2 rule
    // applies to it exactly as to the repo: a dest below it resolves
    // through content the payload can plant symlinks in.
    let mut repo = synth_repo();
    repo.worktrees = Some(PathBuf::from("/synth/repo__worktrees"));
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/synth/repo__worktrees/sub"),
        Mode::Rw,
    ));

    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
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
    argv.windows(2)
        .position(|w| w[0] == "--ro-bind" && w[1] == src)
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
fn the_nix_daemon_rides_with_the_network() {
    // Review-1 finding 6 bound /nix/var/nix (store database, daemon
    // socket) unconditionally so the `nix` on the sandbox PATH works.
    // Review-2 item 3 ties it to the network switch: a read-only bind
    // does not stop the payload from connecting to the daemon, and the
    // daemon builds fixed-output derivations, which keep network
    // access — so under `network = false` it must be absent.
    let shared = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    let share_net = shared
        .iter()
        .position(|a| a == "--share-net")
        .expect("--share-net");
    let var_nix = pos_pair(&shared, "--ro-bind-try", "/nix/var/nix");
    let store = pos_pair(&shared, "--ro-bind", "/nix/store");
    assert!(share_net < var_nix, "the daemon comes with --share-net");
    assert!(
        var_nix < store,
        "the network section precedes the base binds"
    );

    let denied = bwrap_argv(
        &base(false),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert!(
        !denied.contains(&"/nix/var/nix".to_string()),
        "no daemon socket under a denied network: {denied:?}"
    );
    // The store itself stays readable either way: running the shipped
    // tools needs it, and it exposes no daemon.
    assert!(denied.contains(&"/nix/store".to_string()));
}

#[test]
fn the_host_nix_conf_is_never_bound() {
    // Review-2 item 3: the host's /etc/nix/nix.conf may carry
    // `access-tokens`; a read-only bind hands them to the payload.
    // Without a pinned replacement, the sandbox simply has no nix
    // configuration.
    for network in [true, false] {
        let argv = bwrap_argv(
            &base(network),
            &synth_repo(),
            &Payload::Shell,
            &host_env(&[]),
            &params(),
        )
        .unwrap();
        assert!(
            !argv.contains(&"/etc/nix/nix.conf".to_string()),
            "network={network}: {argv:?}"
        );
    }
}

#[test]
fn a_pinned_sanitized_nix_conf_is_bound_read_only() {
    // The wrapper generates a minimal client configuration and pins
    // it; mysbx binds THAT at /etc/nix/nix.conf, right after the base
    // binds.
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: Some("/synth/store/mysbx-nix.conf"),
        ca_bundle: None,
        policy_paths: &[],
        mux_entry: None,
    };
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    let at = argv
        .windows(3)
        .position(|w| {
            w[0] == "--ro-bind"
                && w[1] == "/synth/store/mysbx-nix.conf"
                && w[2] == "/etc/nix/nix.conf"
        })
        .expect("the pinned nix.conf is bound");
    let localtime = pos_pair(&argv, "--ro-bind", "/etc/localtime");
    let repo_bind = pos_pair(&argv, "--bind", "/synth/repo");
    assert!(localtime < at && at < repo_bind, "after the base binds");
}

#[test]
fn a_pinned_bin_sh_is_bound_read_only_into_the_empty_root() {
    // tmux runs every run-shell/if-shell/#() job through
    // `execl("/bin/sh", ...)` — tmux >= 3.5a hardcodes _PATH_BSHELL
    // for jobs (default-shell covers panes and popups only) — and the
    // minimal root has no /bin at all. The pin turns that ABI back
    // on; the bind sits with the base binds, before the repo bind.
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: Some("/synth/bin/sh"),
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &[],
        mux_entry: None,
    };
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    let at = argv
        .windows(3)
        .position(|w| w[0] == "--ro-bind" && w[1] == "/synth/bin/sh" && w[2] == "/bin/sh")
        .expect("the pinned /bin/sh is bound");
    let localtime = pos_pair(&argv, "--ro-bind", "/etc/localtime");
    let repo_bind = pos_pair(&argv, "--bind", "/synth/repo");
    assert!(localtime < at && at < repo_bind, "after the base binds");
}

#[test]
fn without_the_bin_sh_pin_no_bin_sh_is_bound() {
    // Unset means "no /bin/sh", never "the host's": like the nix.conf
    // pin, the state an unwrapped build gets is the absence itself.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert!(!argv.contains(&"/bin/sh".to_owned()), "argv: {argv:?}");
}

#[test]
fn a_mount_dest_onto_bin_sh_is_refused() {
    // /bin/sh is a base-bind root: a configured mount may not shadow
    // the shell every job-spawning tool in the sandbox agrees on —
    // neither when the pin bound one (that is the shadow case) nor
    // when it did not (the dest is refused regardless, exactly like
    // /etc/nix/nix.conf below a wrapper that pins no nix.conf).
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/other-shell", Some("/bin/sh"), Mode::Rw));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::ProtectedDest { protected, .. }
            if protected == "/bin/sh"),
        "wrong error: {err}"
    );
}

#[test]
fn a_mount_dest_of_bin_itself_is_refused_as_the_shadows_ancestor() {
    // `/bin` is not a protected root itself, but it is an ANCESTOR of
    // the protected `/bin/sh`: a mount over `/bin` would hide the
    // pinned shell and let a sidecar replace it with its own — the
    // two-direction `check_dest` rule refuses it, the same way `/nix`
    // is refused over protected `/nix/store`.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/bin2", Some("/bin"), Mode::Ro));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::ProtectedDest { protected, .. }
            if protected == "/bin/sh"),
        "wrong error: {err}"
    );
}

#[test]
fn mount_dest_onto_nix_var_is_refused() {
    const EXPECTED: &str = "would shadow or overwrite the protected";
    // The base's nix paths are protected like every base bind.
    let mut cfg = base(false);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/nix/var/nix"), Mode::Rw));

    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
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

    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
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
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/etc/nix/other.conf"),
        Mode::Ro,
    ));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
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
    // below it.
    let repo = worktree_repo(&["/synth/main/.git/worktrees/wt"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params()).unwrap();
    assert!(pos_pair(&argv, "--bind", "/synth/main/.git/worktrees/wt") > 0);
}

#[test]
fn exact_approval_is_enough() {
    let repo = worktree_repo(&["/synth/target"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/target")];
    let argv = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params()).unwrap();
    assert!(pos_pair(&argv, "--bind", "/synth/target") > 0);
}

#[test]
fn sibling_approval_does_not_cover() {
    // Component-boundary containment: `/synth/targets` does not approve
    // `/synth/target`.
    let repo = worktree_repo(&["/synth/target"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/targets")];
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
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
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(
            err,
            mysbx::bwrap::Error::GitDirProtected { protected: "/", .. }
        ),
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
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
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
    cfg.mounts.push(make_mount(
        "/synth/repo/tools",
        Some("/opt/tools"),
        Mode::Ro,
    ));
    cfg.mounts
        .push(make_mount("/synth/data", Some("/opt/tools/x"), Mode::Ro));
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
fn a_read_only_reexposure_of_a_writable_mount_is_writable_too() {
    // Same one layer out: the rw mount makes its SOURCE writable, and
    // a later ro mount of a path inside that source inherits the
    // property.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/work", Some("/work"), Mode::Rw));
    cfg.mounts
        .push(make_mount("/synth/work/sub", Some("/opt/sub"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/data", Some("/opt/sub/x"), Mode::Ro));
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
fn a_read_only_mount_of_ordinary_host_state_stays_a_usable_parent() {
    // The carve-out that keeps ro nesting usable: a declared host path
    // outside every writable tree cannot be rewritten from inside the
    // sandbox, so a dest below it is allowed.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/etc", Some("/opt/etc"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/data", Some("/opt/etc/x"), Mode::Ro));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
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
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
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
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert!(argv.contains(&"/mysbx-home/.gitconfig".to_string()));
}

// ---- the sandbox home is infrastructure too (review-2 item 5) --------------

#[test]
fn mount_dest_on_the_sandbox_home_is_refused() {
    // Replacing the tmpfs with a host directory would leave HOME
    // pointing at content no layer declared, while the report still
    // claims the fresh tmpfs exists.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some(SANDBOX_HOME), Mode::Rw));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(
            err,
            mysbx::bwrap::Error::ProtectedDest {
                protected: SANDBOX_HOME,
                ..
            }
        ),
        "wrong error: {err}"
    );
}

#[test]
fn a_root_dest_keeps_the_sharper_root_diagnosis() {
    // The ancestor direction matters too, but `/mysbx-home` sits
    // directly under `/`: on component boundaries its only ancestor is
    // the root, which the protected list already refuses. (`/mysbx` is
    // a string prefix, not an ancestor — a different directory, like
    // `/usr/bin2` beside `/usr/bin`; the test below keeps it usable.)
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/"), Mode::Ro));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(
            err,
            mysbx::bwrap::Error::ProtectedDest { protected: "/", .. }
        ),
        "wrong error: {err}"
    );
}

#[test]
fn a_dest_named_like_a_parent_of_the_sandbox_home_stays_allowed() {
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/mysbx"), Mode::Ro));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
}

#[test]
fn seeding_below_the_sandbox_home_stays_allowed() {
    // The one-directional rule: strict descendants are the documented
    // way to seed dotfiles (config.md D14).
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/dotfiles/gitconfig",
        Some("/mysbx-home/.config/git"),
        Mode::Ro,
    ));
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    assert!(argv.contains(&"/mysbx-home/.config/git".to_string()));
}

#[test]
fn a_sandbox_home_lookalike_dest_stays_allowed() {
    // Component-exact, like every other protected path: `/mysbx-homey`
    // is a different directory.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/mysbx-homey"), Mode::Ro));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
}

#[test]
fn the_sandbox_home_sits_directly_under_the_root() {
    // Several claims depend on this: that the only component-wise
    // ancestor of the sandbox home is `/` (already protected), that a
    // dest of `/mysbx` is a lookalike rather than a parent, and that
    // no WRITABLE bind can sit between `/` and the tmpfs. If the
    // constant ever moves deeper, those must be revisited — so fail
    // here rather than silently in the guards.
    assert_eq!(
        Path::new(SANDBOX_HOME).parent(),
        Some(Path::new("/")),
        "SANDBOX_HOME moved: revisit the one-directional check in check_dest"
    );
}

#[test]
fn redundant_spellings_of_the_sandbox_home_are_refused() {
    // Same normalization bar as every protected path: `.` runs, `..`
    // climbs and duplicate slashes must not spell a way onto the
    // tmpfs.
    for dest in [
        "/mysbx-home/.",
        "/mysbx-home/sub/..",
        "//mysbx-home",
        "/mysbx-home/./sub/../",
    ] {
        let mut cfg = base(true);
        cfg.mounts
            .push(make_mount("/synth/data", Some(dest), Mode::Rw));
        let err = bwrap_argv(
            &cfg,
            &synth_repo(),
            &Payload::Shell,
            &host_env(&[]),
            &params(),
        )
        .expect_err("must be refused");
        assert!(
            matches!(
                err,
                mysbx::bwrap::Error::ProtectedDest {
                    protected: SANDBOX_HOME,
                    ..
                }
            ),
            "dest {dest}: wrong error: {err}"
        );
    }
}

#[test]
fn a_git_dir_at_the_sandbox_home_is_refused_as_protected() {
    // The git-dir guard runs the same check_dest, so the sandbox home
    // is out of reach for a `.git` pointer too.
    let repo = worktree_repo(&[SANDBOX_HOME]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from(SANDBOX_HOME)];
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(
            err,
            mysbx::bwrap::Error::GitDirProtected {
                protected: SANDBOX_HOME,
                ..
            }
        ),
        "wrong error: {err}"
    );
}

#[test]
fn a_mount_may_not_source_the_daemon_under_a_denied_network() {
    // The daemon is a network service: binding its socket back in
    // through an ordinary mount would undo `network = false` no matter
    // where the dest points (review-2 item 3).
    let mut cfg = base(false);
    cfg.mounts.push(make_mount(
        "/nix/var/nix/daemon-socket",
        Some("/opt/socket"),
        Mode::Ro,
    ));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DaemonUnderDeniedNetwork { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_mount_may_source_the_daemon_when_the_network_is_shared() {
    // With the network shared the daemon is bound anyway, so an
    // explicit mount adds nothing to refuse.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/nix/var/nix/daemon-socket",
        Some("/opt/socket"),
        Mode::Ro,
    ));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
}

// ---- order-independent writable-alias analysis (review-3 item 1) -----------

#[test]
fn a_ro_alias_declared_before_the_rw_alias_is_caught() {
    // Review-3 item 1, first miss: an `ro` alias is declared BEFORE
    // the `rw` mount of a path inside it. The forward scan used to see
    // the ro mount first, conclude "ordinary host state", and clear it
    // as a safe parent — leaving a dest below the alias resolvable
    // through a payload-planted symlink.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/host/tree", Some("/view"), Mode::Ro));
    cfg.mounts.push(make_mount(
        "/synth/host/tree/writable",
        Some("/w"),
        Mode::Rw,
    ));
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/view/writable/jump"),
        Mode::Ro,
    ));
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
fn a_ro_parent_containing_the_repo_is_not_a_safe_parent() {
    // Review-3 item 1, second miss: an `ro` mount of a tree CONTAINING
    // the repo. The repo is always rw (D13), so the alias re-exposes
    // writable content — a dest below `/view` is resolvable through a
    // symlink planted in the work tree, no matter that the alias
    // itself is `ro` and declared first.
    let repo = synth_repo(); // root: /synth/repo
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth", Some("/view"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/data", Some("/view/repo/jump"), Mode::Ro));
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params())
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_ro_parent_containing_an_rw_mount_is_not_a_safe_parent() {
    // The old `mount_order_is_preserved` fixture — ro outer, rw inner —
    // becomes a refusal (review-3 item 1): the ro alias re-exposes
    // content the payload can write through the rw bind, so a dest
    // below it resolves through whatever symlink the payload planted
    // there between runs.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data/outer", None, Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/data/outer/nested", None, Mode::Rw));
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
fn a_ro_parent_containing_a_git_dir_is_not_a_safe_parent() {
    // Same shape with the git metadata instead of the work tree: the
    // git dirs are rw and symlink-plantable, so an ro alias above them
    // inherits the property.
    let repo = worktree_repo(&["/synth/main/.git/worktrees/wt"]);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    cfg.mounts
        .push(make_mount("/synth/main", Some("/view"), Mode::Ro));
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/view/.git/worktrees/wt/hooks"),
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
fn a_ro_chain_of_aliases_over_writable_content_is_caught() {
    // A three-hop chain exercises more than one propagation pass of
    // the fixed point: ro alias of an ro alias of a tree containing an
    // rw mount. Pass 1 learns the rw source; pass 2 marks the middle
    // alias writable; pass 3 marks the outer one — only then is the
    // dest below the outer alias refused. A forward scan or a
    // single-pass overlap would let it through.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/host/a/b", Some("/hop2"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/host/a", Some("/hop1"), Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/host/a/b/writable", Some("/w"), Mode::Rw));
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/hop1/b/writable/jump"),
        Mode::Ro,
    ));
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

// ---- the daemon guard checks both directions (review-3 item 2) -------------

#[test]
fn a_read_only_ancestor_of_the_nix_daemon_dir_is_refused() {
    // The review's exact example: binding `/nix` somewhere else still
    // exposes `/nix/var/nix/daemon-socket/socket` through the wider
    // window, read-only or not — the socket only needs to be
    // connectable, not writable.
    let mut cfg = base(false);
    cfg.mounts
        .push(make_mount("/nix", Some("/host-nix"), Mode::Ro));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DaemonUnderDeniedNetwork { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn the_whole_host_root_is_refused_under_a_denied_network() {
    // The extreme ancestor: `path = "/"` binds everything, daemon
    // included. (`check_dest` refuses the ROOT as a DEST on its own;
    // the source side is this guard's job.)
    let mut cfg = base(false);
    cfg.mounts
        .push(make_mount("/", Some("/host-root"), Mode::Ro));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DaemonUnderDeniedNetwork { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn an_unrelated_nix_store_source_stays_mountable_without_the_network() {
    // The carve-out that keeps the guard usable: `/nix/store` itself
    // does not contain `/nix/var/nix` (and `/nix/store` is bound by
    // the base table regardless), so a mount of it stays allowed.
    let mut cfg = base(false);
    cfg.mounts
        .push(make_mount("/nix/store/extra", Some("/opt/extra"), Mode::Ro));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
}

// ---- trusted policy files stay out of writable binds (review-3 item 3) ----

#[test]
fn a_relocated_writable_parent_of_the_sidecar_is_refused() {
    // The review's scenario: for repo ~/src/r the sidecar is
    // ~/src/r.mysbx/config.toml. An `rw` mount of ~/src — relocated or
    // not — makes that file payload-writable, and a writable sidecar
    // steers the NEXT run: `git-dirs` approvals can be added, the
    // `.git` pointer rewritten to match.
    let repo = synth_repo(); // root /synth/repo, sidecar /synth/repo.mysbx
    let policy = [mysbx::bwrap::PolicyPath::lexical(
        "/synth/repo.mysbx/config.toml",
    )];
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &policy,
        mux_entry: None,
    };
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth", Some("/all-src"), Mode::Rw));
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params)
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_writable_mount_of_the_sidecar_directory_itself_is_refused() {
    // No relocation needed: an `rw` mount that sources the sidecar
    // directory directly is the same hole, dest aside.
    let repo = synth_repo();
    let policy = [mysbx::bwrap::PolicyPath::lexical(
        "/synth/repo.mysbx/config.toml",
    )];
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &policy,
        mux_entry: None,
    };
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/repo.mysbx", Some("/policy"), Mode::Rw));
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params)
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_read_only_mount_of_the_sidecar_stays_allowed() {
    // The carve-out: `ro` cannot write the file in place, so reviewing
    // the sidecar from inside the sandbox stays possible. (A symlink
    // planted in it is the accident barrier, D9 — and no dest below it
    // is allowed anyway, by the writable-alias rule.)
    let repo = synth_repo();
    let policy = [mysbx::bwrap::PolicyPath::lexical(
        "/synth/repo.mysbx/config.toml",
    )];
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &policy,
        mux_entry: None,
    };
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/repo.mysbx", Some("/policy"), Mode::Ro));
    bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params).unwrap();
}

#[test]
fn a_writable_mount_unrelated_to_the_policy_files_stays_allowed() {
    // Ordinary rw grants elsewhere on the host are the feature, not
    // the hole: only a source CONTAINING a policy file is refused.
    let repo = synth_repo();
    let policy = [mysbx::bwrap::PolicyPath::lexical(
        "/synth/repo.mysbx/config.toml",
    )];
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &policy,
        mux_entry: None,
    };
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/work", Some("/work"), Mode::Rw));
    bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params).unwrap();
}

#[test]
fn the_implicit_repo_bind_exposing_a_policy_file_is_refused() {
    // The repo bind is rw too: a user config that lives inside the
    // work tree (or a sidecar nested into it, however that happened)
    // is refused rather than silently exposed. This covers the case
    // the review spelled "including the implicit repo bind" for the
    // daemon — same reasoning, different protected path.
    let repo = synth_repo();
    let policy = [mysbx::bwrap::PolicyPath::lexical("/synth/repo/config.toml")];
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &policy,
        mux_entry: None,
    };
    let cfg = base(true);
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params)
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_git_dir_exposing_a_policy_file_is_refused() {
    // Git metadata is rw as well (D13); an approved dir containing a
    // policy file is the same widening hole.
    let repo = worktree_repo(&["/synth/main/.git/worktrees/wt"]);
    let policy = [mysbx::bwrap::PolicyPath::lexical(
        "/synth/main/.git/worktrees/wt/config.toml",
    )];
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &policy,
        mux_entry: None,
    };
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params)
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn an_absent_policy_file_does_not_forbid_its_would_be_parent() {
    // The existence filter, documented: an absent config granted
    // nothing, so an `rw` source containing its would-be location is
    // allowed THIS run. The guarantee is temporal, not lexical: the
    // payload can create the file there, and the run that follows
    // refuses the same `rw` source (the file then exists).
    let repo = synth_repo();
    let params = Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &[], // nothing exists -> nothing protected
        mux_entry: None,
    };
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth", Some("/all-src"), Mode::Rw));
    bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params).unwrap();
}

// ---- the policy PATHNAME is protected too (review-4 item 1) ----------

/// A policy file the way Home Manager writes it: the pathname
/// `<dir>/config.toml` is a symlink whose target lives in the
/// immutable store. Both halves are guarded — that is exactly what
/// `mysbx::trusted_policy` produces for such a file.
fn hm_style_policy(pathname: &str, target: &str) -> mysbx::bwrap::PolicyPath {
    mysbx::bwrap::PolicyPath {
        path: PathBuf::from(pathname),
        guarded: vec![PathBuf::from(pathname), PathBuf::from(target)],
    }
}

fn params_with(policy: &[mysbx::bwrap::PolicyPath]) -> Params<'_> {
    Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: policy,
        mux_entry: None,
    }
}

#[test]
fn a_writable_mount_over_a_policy_symlink_is_refused_although_the_target_is_elsewhere() {
    // The review-4 exploit: the resolved target sits in /nix/store and
    // no writable bind can touch it — but the SYMLINK that names it is
    // in an ordinary directory, and an rw mount of that directory lets
    // the payload replace it. The next run would then read the
    // attacker's policy.
    let policy = [hm_style_policy(
        "/synth/home/.config/mysbx/config.toml",
        "/nix/store/aaaa-mysbx-config.toml",
    )];
    let params = params_with(&policy);
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/home/.config/mysbx",
        Some("/policy"),
        Mode::Rw,
    ));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_writable_mount_over_a_traversed_directory_is_refused() {
    // Not only the final entry: a writable bind of any directory on
    // the pathname can rename it out of the way and put a new chain in
    // its place.
    let policy = [hm_style_policy(
        "/synth/home/.config/mysbx/config.toml",
        "/nix/store/aaaa-mysbx-config.toml",
    )];
    let params = params_with(&policy);
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/home/.config", Some("/cfg"), Mode::Rw));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn the_resolved_target_stays_protected_as_well() {
    // The original (review-3) check is kept, not replaced: an rw
    // source containing the TARGET is refused even when the pathname
    // is untouched.
    let policy = [hm_style_policy(
        "/synth/home/.config/mysbx/config.toml",
        "/synth/generated/mysbx-config.toml",
    )];
    let params = params_with(&policy);
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/generated", Some("/gen"), Mode::Rw));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn the_repo_bind_covering_a_policy_pathname_is_refused() {
    // The implicit repo bind is rw by definition (D13): a policy
    // pathname inside the work tree is refused there too, target
    // elsewhere or not.
    let policy = [hm_style_policy(
        "/synth/repo/.mysbx-config.toml",
        "/nix/store/aaaa-mysbx-config.toml",
    )];
    let params = params_with(&policy);
    let cfg = base(true);
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_git_dir_covering_a_policy_pathname_is_refused() {
    // Approved git metadata is bound rw as well (review-1 finding 4).
    let repo = worktree_repo(&["/synth/main/.git/worktrees/wt"]);
    let policy = [hm_style_policy(
        "/synth/main/.git/worktrees/wt/config.toml",
        "/nix/store/aaaa-mysbx-config.toml",
    )];
    let params = params_with(&policy);
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    let err = bwrap_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params)
        .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn an_unrelated_writable_mount_stays_allowed_with_a_symlinked_policy() {
    // The guard stays narrow: neither the pathname chain nor the
    // target lies below this source, so it is an ordinary rw grant.
    let policy = [hm_style_policy(
        "/synth/home/.config/mysbx/config.toml",
        "/nix/store/aaaa-mysbx-config.toml",
    )];
    let params = params_with(&policy);
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/work", Some("/work"), Mode::Rw));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
}

#[test]
fn a_read_only_mount_of_a_policy_pathname_stays_allowed() {
    // `ro` cannot replace a directory entry either: reviewing the
    // generated user config from inside the sandbox stays possible.
    let policy = [hm_style_policy(
        "/synth/home/.config/mysbx/config.toml",
        "/nix/store/aaaa-mysbx-config.toml",
    )];
    let params = params_with(&policy);
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/home/.config/mysbx",
        Some("/policy"),
        Mode::Ro,
    ));
    bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
}
