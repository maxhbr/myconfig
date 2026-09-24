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

use mysbx::bwrap::{bwrap_argv, HostEnv, Params, Payload, Workspace, SANDBOX_HOME};
use mysbx::config::{Display, Mode, Mount, Multiplexer};
use mysbx::merge::Merged;
use mysbx::nono::{nono_run_argv, Params as NonoParams};
use mysbx::podman_gvisor::CONTAINER_HOME;
use mysbx::podman_gvisor::{podman_run_argv, Params as PodmanParams};
use mysbx::repo::Repo;
use std::borrow::Cow;
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
        forward_env: Vec::new(),
        allow_domains: Vec::new(),
        connect_ports: Vec::new(),
        listen_ports: Vec::new(),
        multiplexer: Multiplexer::None,
        display: Display::Off,
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
        waypipe: None,
        workspace: Workspace::Live,
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

/// All `(flag, source, dest)` bind triples — the MODE-sensitive view:
/// which of `--ro-bind`/`--bind` a pair was emitted with decides what
/// the payload can write through it.
fn bind_triples(argv: &[String]) -> Vec<(&str, &str, &str)> {
    argv.windows(3)
        .filter(|w| w[0] == "--ro-bind" || w[0] == "--bind")
        .map(|w| (w[0].as_str(), w[1].as_str(), w[2].as_str()))
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
        forward_env: Vec::new(),
        allow_domains: Vec::new(),
        connect_ports: Vec::new(),
        listen_ports: Vec::new(),
        multiplexer: Multiplexer::None,
        display: Display::Off,
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
/// (herdr's entry started for `orca`, say) a visible diff.
fn mux_case(mux: Multiplexer) -> (&'static str, &'static str) {
    match mux {
        Multiplexer::Tmux => ("/synth/bin/mysbx-tmux-entry", "mux-tmux-shell.txt"),
        Multiplexer::Workmux => ("/synth/bin/mysbx-workmux-entry", "mux-workmux-shell.txt"),
        Multiplexer::Herdr => ("/synth/bin/mysbx-herdr-entry", "mux-herdr-shell.txt"),
        Multiplexer::Aoe => ("/synth/bin/mysbx-aoe-entry", "mux-aoe-shell.txt"),
        Multiplexer::Orca => ("/synth/bin/mysbx-orca-entry", "mux-orca-shell.txt"),
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
    // running the same assertions over all five.
    for mux in [
        Multiplexer::Tmux,
        Multiplexer::Workmux,
        Multiplexer::Herdr,
        Multiplexer::Aoe,
        Multiplexer::Orca,
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
        Multiplexer::Orca,
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

// ---- the display channel (docs/design/config.md D18) --------------------

/// The waypipe pin of a display test: one socket directory, one guest
/// binary — the shapes the wrapper would pin.
fn waypipe_params() -> Params<'static> {
    let mut p = params();
    p.waypipe = Some(mysbx::bwrap::Waypipe {
        socket_dir: "/synth/repo.mysbx/waypipe/1234",
        guest_bin: "/synth/bin/waypipe",
    });
    p
}

#[test]
fn golden_waypipe_display_shell() {
    // D18: an INTERACTIVE run with `display = "waypipe"` binds the
    // per-run socket directory rw, pins `XDG_RUNTIME_DIR` at the
    // tmpfs home and wraps the shell in the guest waypipe server.
    let mut cfg = base(true);
    cfg.display = Display::Waypipe;
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &waypipe_params(),
    )
    .unwrap();
    assert_golden("display-waypipe-shell.txt", &argv);
}

#[test]
fn golden_waypipe_display_run() {
    // D18: the display is NOT mux-like — a one-shot `run -- CMD` is
    // wrapped too, because a command that opens a window needs the
    // channel just as much as the interactive shell.
    let mut cfg = base(true);
    cfg.display = Display::Waypipe;
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Command(vec!["ls".into(), "-x".into()]),
        &host_env(&[]),
        &waypipe_params(),
    )
    .unwrap();
    assert_golden("display-waypipe-run.txt", &argv);
}

#[test]
fn display_off_is_byte_identical_to_the_pre_d18_argv() {
    // The byte-compat contract: `display = "off"` (and the key absent
    // from both layers) gives exactly the pre-existing argv, even
    // with a waypipe pin present — the pin is not an opt-in by
    // itself, the config key is.
    let mut cfg = base(true);
    cfg.display = Display::Off;
    let argv = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &waypipe_params(),
    )
    .unwrap();
    assert_golden("interactive-shell.txt", &argv);
    assert!(!argv.iter().any(|a| a.contains("waypipe")));
    assert!(!argv.iter().any(|a| a == "XDG_RUNTIME_DIR"));
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
    // `host_env` (`OPENAI_API_KEY` here stands in for a credential the
    // caller never handed in; this test passes an empty host_env).
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
        forward_env: Vec::new(),
        allow_domains: Vec::new(),
        connect_ports: Vec::new(),
        listen_ports: Vec::new(),
        multiplexer: Multiplexer::None,
        display: Display::Off,
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
    assert!(
        argv.windows(2).any(|w| w[0] == "--tmpfs" && w[1] == "/tmp"),
        "/tmp must be a tmpfs: {argv:?}"
    );
    for w in argv.windows(3) {
        assert!(
            !((w[0] == "--bind" || w[0] == "--ro-bind") && w[2] == "/tmp"),
            "/tmp must not be bind-backed: {:?}",
            argv
        );
    }
}

#[test]
fn dev_shm_is_a_tmpfs_of_its_own() {
    // bubblewrap's `--dev` creates no /dev/shm, so POSIX shared memory
    // is unusable without this row of the base table — a chrome-family
    // browser crashes outright. It sits AFTER `--dev /dev`, otherwise
    // the devtmpfs would cover it again, and it is a tmpfs, so nothing
    // of the host's /dev/shm is reachable.
    let argv = bwrap_argv(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    let dev = argv.iter().position(|x| x.as_str() == "--dev").unwrap();
    let shm = argv
        .windows(2)
        .position(|w| w[0] == "--tmpfs" && w[1] == "/dev/shm")
        .expect("/dev/shm must be a tmpfs");
    assert!(dev < shm, "/dev/shm must come after --dev /dev: {argv:?}");
    for (src, dest) in bind_pairs(&argv) {
        assert_ne!(dest, "/dev/shm", "/dev/shm is bind-backed by {src}");
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
    assert_eq!(tmpfs, vec!["/dev/shm", "/tmp", SANDBOX_HOME]);
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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
        waypipe: None,
        workspace: Workspace::Live,
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

// ---- the clone sessions of the workspace model (workspace.md D1-D5) --------

/// The session clone of the synthetic repo, the path `--session fix-1`
/// would derive: `<sidecar>/clones/fix-1`.
const SYNTH_CLONE: &str = "/synth/repo.mysbx/clones/fix-1";

/// `Params` with the workspace switched to the clone of the synthetic
/// repo — the argv a `--session fix-1` run builds.
fn clone_params() -> Params<'static> {
    Params {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        bin_sh: None,
        nix_conf: None,
        ca_bundle: None,
        policy_paths: &[],
        mux_entry: None,
        waypipe: None,
        workspace: Workspace::Clone {
            clone: Path::new(SYNTH_CLONE),
        },
    }
}

/// The worst-case LIVE repo for the absence tests: external git dirs,
/// a worktrees sibling AND state-dirs declared — a clone run must
/// drop every one of those binds.
fn laden_repo() -> Repo {
    Repo {
        root: PathBuf::from("/synth/repo"),
        sidecar: PathBuf::from("/synth/repo.mysbx"),
        git_dirs: vec![PathBuf::from("/synth/main/.git")],
        worktrees: Some(PathBuf::from("/synth/repo__worktrees")),
    }
}

fn laden_cfg() -> Merged {
    let mut cfg = base(true);
    cfg.git_dirs = vec![PathBuf::from("/synth/main/.git")];
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.mounts
        .push(make_mount("/synth/data/refs", None, Mode::Ro));
    cfg.mounts
        .push(make_mount("/synth/data/cache", Some("/cache"), Mode::Rw));
    cfg
}

#[test]
fn a_clone_run_binds_the_clone_rw_at_the_repo_path() {
    // D3, the core shape: the clone is bound rw AT THE REPO'S OWN
    // PATH — path identity is preserved, and the payload cannot tell
    // the bind from the real checkout. The `--chdir` stays the repo
    // path for the same reason.
    let repo = laden_repo();
    let argv = bwrap_argv(
        &laden_cfg(),
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &clone_params(),
    )
    .unwrap();
    let pairs = bind_pairs(&argv);
    // The rw bind of the clone at the repo path — flag `--bind`,
    // source the clone, dest the repo path.
    let triples = bind_triples(&argv);
    assert!(
        triples.contains(&("--bind", SYNTH_CLONE, "/synth/repo")),
        "the clone must be bound rw at the repo's own path: {triples:?}"
    );
    // The bind's SOURCE is the clone, exactly the clone directory —
    // not the sidecar, not `clones/`.
    assert!(
        pairs.contains(&(SYNTH_CLONE, "/synth/repo")),
        "the rw bind at the repo path must source the clone: {pairs:?}"
    );
    // `--chdir` still lands in the repo path.
    let chdir = argv
        .iter()
        .position(|a| a == "--chdir")
        .expect("--chdir present");
    assert_eq!(argv[chdir + 1], "/synth/repo");
}

#[test]
fn a_clone_run_mounts_nothing_else_of_the_host_repo() {
    // D3: the host repo, the `__worktrees` sibling, the git metadata
    // directories and the state-dir binds of a live run are ALL
    // absent from a clone run's argv — the clone is the only thing
    // bound at the repo path, and it carries its own `.git`
    // directory, so there is nothing external to approve or bind.
    let repo = laden_repo();
    let argv = bwrap_argv(
        &laden_cfg(),
        &repo,
        &Payload::Shell,
        &host_env(&[]),
        &clone_params(),
    )
    .unwrap();
    let pairs = bind_pairs(&argv);
    // The host repo appears only as the clone bind's DEST, never as a
    // source; the git dir, the worktrees sibling and the state
    // backing store appear nowhere at all.
    for forbidden in [
        ("/synth/repo", "the host repo"),
        ("/synth/main/.git", "a git metadata directory"),
        ("/synth/repo__worktrees", "the worktrees sibling"),
        (
            "/synth/repo.mysbx/state/.local/share/opencode",
            "a state-dir backing store",
        ),
    ] {
        assert!(
            !pairs.iter().any(|(src, _)| *src == forbidden.0),
            "{} must not be bound in a clone run: {pairs:?}",
            forbidden.1
        );
    }
    // The in-sandbox state dest is absent too.
    assert!(!argv.contains(&format!("{}/.local/share/opencode", SANDBOX_HOME)));
    // No source of any bind sits inside the host repo except through
    // the clone (whose path lives in the SIDECAR, not the repo).
    assert!(
        pairs
            .iter()
            .all(|(src, _)| !src.starts_with("/synth/repo/") && *src != "/synth/repo"),
        "the host repo tree must be unreachable: {pairs:?}"
    );
}

#[test]
fn a_clone_run_forces_every_configured_mount_ro() {
    // D4: every `[[mounts]]` entry is downgraded to read-only — the
    // rw cache entry of the laden fixture binds with `--ro-bind`, the
    // ro one stays ro. The downgrade applies to the configured mounts
    // only: the clone keeps its `--bind`.
    let argv = bwrap_argv(
        &laden_cfg(),
        &laden_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &clone_params(),
    )
    .unwrap();
    let triples = bind_triples(&argv);
    assert!(
        triples.contains(&("--ro-bind", "/synth/data/refs", "/synth/data/refs")),
        "the ro entry keeps its mode: {triples:?}"
    );
    assert!(
        triples.contains(&("--ro-bind", "/synth/data/cache", "/cache")),
        "the rw entry must be downgraded to `--ro-bind`: {triples:?}"
    );
    assert!(
        !triples.contains(&("--bind", "/synth/data/cache", "/cache")),
        "no configured mount may stay writable in a clone run: {triples:?}"
    );
    // The clone bind stays rw — the only writable one (D4).
    assert!(
        triples.contains(&("--bind", SYNTH_CLONE, "/synth/repo")),
        "{triples:?}"
    );
}

#[test]
fn golden_clone_session() {
    // Byte-for-byte: the laden fixture as a clone run — the clone at
    // the repo path, both mounts forced ro, no git/worktrees/state
    // binds, the payload unchanged. The golden file is the reviewable
    // statement of D3's "nothing else of the host repo".
    let argv = bwrap_argv(
        &laden_cfg(),
        &laden_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &clone_params(),
    )
    .unwrap();
    assert_golden("clone-session.txt", &argv);
}

#[test]
fn a_clone_run_still_refuses_a_dest_below_the_repo_path() {
    // The guards keep holding with the clone as the writable source:
    // a dest below the repo path resolves through content the payload
    // can write (the clone), so review-2 item 2 applies unchanged.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/synth/repo/jump"),
        Mode::Ro,
    ));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &clone_params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_clone_run_still_refuses_a_mount_hiding_the_repo_path() {
    // check_hidden_mounts: the implicit bind's dest is the repo path
    // (the clone binds THERE), so a configured mount covering it is
    // refused like one covering the live repo bind.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/synth"), Mode::Ro));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &clone_params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::HiddenMount { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn a_ro_mount_sourcing_the_clone_contributes_its_dest_to_the_writable_set() {
    // The ro-alias rule (review-3 item 1) holds in a clone run too: a
    // read-only mount whose SOURCE is inside the clone re-exposes
    // writable content (the same host inode is writable through the
    // clone bind), so a dest below IT is refused as symlink-plantable.
    let mut cfg = base(true);
    cfg.mounts.push(make_mount(
        &format!("{SYNTH_CLONE}/tools"),
        Some("/synth/aliased"),
        Mode::Ro,
    ));
    cfg.mounts.push(make_mount(
        "/synth/data",
        Some("/synth/aliased/sub"),
        Mode::Ro,
    ));
    let err = bwrap_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &clone_params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::bwrap::Error::DestBelowWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn live_mounts_are_unchanged_by_the_workspace_enum() {
    // D1: a run without `--session` is byte-identical to today's argv.
    // The laden fixture in live mode keeps its rw mount, its git dir,
    // its worktrees bind and its state binds — pinning that the clone
    // branch of `bwrap_argv` did not leak into the default.
    let argv = bwrap_argv(
        &laden_cfg(),
        &laden_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params(),
    )
    .unwrap();
    let pairs = bind_pairs(&argv);
    assert!(pairs.contains(&("/synth/repo", "/synth/repo")));
    assert!(pairs.contains(&("/synth/main/.git", "/synth/main/.git")));
    assert!(pairs.contains(&("/synth/repo__worktrees", "/synth/repo__worktrees")));
    assert!(pairs.contains(&(
        "/synth/repo.mysbx/state/.local/share/opencode",
        "/mysbx-home/.local/share/opencode"
    )));
    assert!(
        pairs.contains(&("/synth/data/cache", "/cache")),
        "{pairs:?}"
    );
}

// ---- session name derivation for multiplexer target naming ----

#[test]
fn window_mode_sets_no_session_name_env() {
    // Window mode (Live workspace) does not set MYSBX_SESSION_NAME.
    // The entry script will use `mysbx-<repo-basename>` as the tmux
    // session name.
    let mut cfg = base(true);
    cfg.multiplexer = Multiplexer::Tmux;
    let mut p = params();
    p.mux_entry = Some("/synth/bin/mysbx-tmux-entry");
    let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &p).unwrap();
    // TMUX_TMPDIR is set, but MYSBX_SESSION_NAME is not.
    let keys = setenv_keys(&argv);
    assert_eq!(keys, vec!["HOME", "PATH", "TMUX_TMPDIR"]);
    assert!(!argv.iter().any(|a| a == "MYSBX_SESSION_NAME"));
}

#[test]
fn session_mode_sets_session_name_env() {
    // Session mode (Clone workspace) sets MYSBX_SESSION_NAME extracted
    // from the clone path. The entry script will use
    // `mysbx-<repo-basename>-<session-name>` as the tmux session name.
    let mut cfg = base(true);
    cfg.multiplexer = Multiplexer::Tmux;
    let mut p = clone_params();
    p.mux_entry = Some("/synth/bin/mysbx-tmux-entry");
    let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &p).unwrap();
    // TMUX_TMPDIR and MYSBX_SESSION_NAME are set.
    let keys = setenv_keys(&argv);
    assert_eq!(
        keys,
        vec!["HOME", "PATH", "TMUX_TMPDIR", "MYSBX_SESSION_NAME"]
    );
    let i = argv
        .iter()
        .position(|a| a == "MYSBX_SESSION_NAME")
        .expect("MYSBX_SESSION_NAME should be set");
    // The session name is extracted from the clone path
    // `/synth/repo.mysbx/clones/fix-1` → `fix-1`
    assert_eq!(argv[i + 1], "fix-1");
}

#[test]
fn session_name_env_works_for_all_multiplexers() {
    // All session-starting multiplexers get the session name env when
    // in session mode.
    for mux in [
        Multiplexer::Tmux,
        Multiplexer::Workmux,
        Multiplexer::Herdr,
        Multiplexer::Aoe,
        Multiplexer::Orca,
    ] {
        let (entry, _) = mux_case(mux);
        let mut cfg = base(true);
        cfg.multiplexer = mux;
        let mut p = clone_params();
        p.mux_entry = Some(entry);
        let argv = bwrap_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env(&[]), &p).unwrap();
        let keys = setenv_keys(&argv);
        assert_eq!(
            keys,
            vec!["HOME", "PATH", "TMUX_TMPDIR", "MYSBX_SESSION_NAME"],
            "{mux}"
        );
        let i = argv
            .iter()
            .position(|a| a == "MYSBX_SESSION_NAME")
            .expect("MYSBX_SESSION_NAME should be set");
        assert_eq!(argv[i + 1], "fix-1", "{mux}");
    }
}

// ---- podman-gvisor backend tests --------------------------------------------

/// A synthetic podman-gvisor params. The shell and tool `PATH` are
/// image paths (bd myconfig-wao) — the container mounts nothing from
/// the host `/nix/store`, so a host store path would die with `no
/// such file or directory`. The synthetic values deliberately differ
/// from the real defaults (`/bin/bash`, `/bin:/usr/bin`) so a test
/// that silently regressed to bwrap's host pins fails the goldens.
fn podman_params() -> PodmanParams<'static> {
    PodmanParams {
        shell: "/bin/synth-shell",
        tools_path: "/bin:/usr/bin:/synth-image-tools",
        policy_paths: &[],
        mux_entry: None,
        waypipe: None,
        workspace: Workspace::Live,
        // The exec'd backend keeps mysbx's stdio, so the container is
        // attached: `--interactive` always (bd myconfig-jho). The
        // synthetic `tty: false` keeps the goldens deterministic —
        // a terminal run's pty is covered by its own test below.
        interactive: true,
        tty: false,
        image: "localhost/agent-gvisor:latest",
        runtime_flags: &[],
        // The rootless default of a wrapped run (lib.rs): cgroupfs is
        // the manager a non-root runsc can actually use.
        cgroup_manager: Some("cgroupfs"),
        ignore_cgroups: false,
        network_spec: None,
        // No backend env pins by default (an unwrapped build sets
        // MYSBX_GVISOR_ENV nowhere); the pin tests pass their own.
        extra_env: &[],
        pids_limit: None,
        memory: None,
        cpus: None,
    }
}

/// A podman-gvisor base config.
fn podman_base(network: bool) -> Merged {
    Merged {
        backend: Some("podman-gvisor".into()),
        network,
        mounts: Vec::new(),
        env: BTreeMap::new(),
        git_dirs: Vec::new(),
        state_dirs: Vec::new(),
        forward_env: Vec::new(),
        allow_domains: Vec::new(),
        connect_ports: Vec::new(),
        listen_ports: Vec::new(),
        multiplexer: Multiplexer::None,
        display: Display::Off,
    }
}

#[test]
fn podman_golden_waypipe_display_shell() {
    // D18 on the podman-gvisor backend: the socket dir is bound rw at
    // itself (a `--mount type=bind`), `XDG_RUNTIME_DIR` points at the
    // container home, and the payload is wrapped in the IN-IMAGE
    // waypipe — the first container arg is waypipe, its own `--`
    // separates its CMD (no extra `--` before the image command).
    let mut cfg = podman_base(true);
    cfg.display = Display::Waypipe;
    let mut params = podman_params();
    params.waypipe = Some(mysbx::bwrap::Waypipe {
        socket_dir: "/synth/repo.mysbx/waypipe/1234",
        guest_bin: "/bin/waypipe",
    });
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    assert_golden("podman-display-waypipe.txt", &argv);
    let image_at = argv.iter().position(|a| a == params.image).unwrap();
    assert_eq!(argv[image_at + 1], "/bin/waypipe");
    assert_eq!(argv[image_at + 2], "--socket");
    // The subcommand comes after its root options, and the payload
    // follows the server's own `--`.
    assert!(argv[image_at + 3..]
        .windows(2)
        .any(|w| w[0] == "server" && w[1] == "--"));
    assert!(
        argv.windows(2).any(|w| w[0] == "--mount"
            && w[1].contains(
                "src=/synth/repo.mysbx/waypipe/1234,dst=/synth/repo.mysbx/waypipe/1234,rw"
            )),
        "the socket dir bind is missing: {argv:?}"
    );
    assert!(
        argv.windows(2)
            .any(|w| w[0] == "--env" && w[1] == "XDG_RUNTIME_DIR=/mysbx-home"),
        "XDG_RUNTIME_DIR must be pinned at the container home: {argv:?}"
    );
}

#[test]
fn podman_golden_minimal_config() {
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    // Args-only argv, like bwrap's: no program name, because lib.rs
    // prepends MYSBX_PODMAN via Command::new. A leading `podman`
    // doubles the program name and podman rejects the first flag
    // (bd myconfig-7c2).
    assert_eq!(argv[0], "--runtime=runsc");
    assert_golden("podman-minimal.txt", &argv);
}

#[test]
fn podman_golden_one_ro_mount() {
    let mut cfg = podman_base(true);
    cfg.mounts
        .push(make_mount("/synth/data/refs", None, Mode::Ro));
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    assert_golden("podman-ro-mount.txt", &argv);
}

#[test]
fn podman_golden_one_rw_mount() {
    let mut cfg = podman_base(true);
    cfg.mounts
        .push(make_mount("/synth/data/cache", None, Mode::Rw));
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    assert_golden("podman-rw-mount.txt", &argv);
}

#[test]
fn podman_golden_network_false() {
    let mut params = podman_params();
    params.network_spec = Some("none");

    let argv = podman_run_argv(
        &podman_base(false),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    assert_golden("podman-network-false.txt", &argv);
    // Network false should set network_spec to "none"
    // Verify --network and none appear consecutively
    assert!(
        argv.windows(2)
            .any(|w| w[0] == "--network" && w[1] == "none"),
        "argv should contain --network none consecutively: {argv:?}"
    );
}

#[test]
fn podman_xdg_base_dirs_derive_from_the_container_home() {
    // bd myconfig-jho: under `--read-only` + `--read-only-tmpfs=true`
    // the container home is writable only where a bind lands, and podman
    // bind copy-up materializes only the FIRST path component of a bind
    // — never the parents of `$HOME/.config`. With the XDG variables
    // unset, tools fell back to `$HOME/<dir>` and died creating history,
    // caches or state under the read-only root; the variables pin the
    // base dirs at the writable (or visibly-mounted) home subpaths. Like
    // `HOME`, they are infrastructure: derived from the container home,
    // emitted after both env layers so no entry can repoint them.
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let pairs: Vec<String> = argv
        .windows(2)
        .filter(|w| w[0] == "--env")
        .map(|w| w[1].clone())
        .collect();

    // The four base dirs sit under the container home — never at the
    // image's own `/home/agent` paths (the read-only root in this
    // backend's shape, i.e. exactly the bug).
    for dir in [".config", ".cache", ".local/state", ".local/share"] {
        let expected = format!("/mysbx-home/{dir}");
        assert!(
            pairs
                .iter()
                .any(|kv| kv.starts_with("XDG_") && kv.ends_with(&format!("={expected}"))),
            "no XDG variable at {expected}: {pairs:?}"
        );
    }
    assert!(
        !pairs.iter().any(|kv| kv.contains("/home/agent")),
        "image home path leaked into the env: {pairs:?}"
    );

    // Infrastructure means LAST: after every host-forwarded and `env`
    // entry, so a later `--env` always wins and an `[env] XDG_*` entry
    // shows up in `--dry-run` but never reaches the payload (the same
    // treatment `HOME`, `PATH` and the CA-bundle variables get).
    let last_xdg = pairs
        .iter()
        .rposition(|kv| kv.starts_with("XDG_"))
        .expect("XDG variables in argv");
    assert!(
        pairs[last_xdg + 1..]
            .iter()
            .all(|kv| kv.starts_with("PATH=")),
        "only PATH may follow the XDG emit: {pairs:?}"
    );
}

#[test]
fn podman_config_env_cannot_repoint_the_xdg_base_dirs() {
    // The infrastructure guard of `HOME`/`PATH` extends to the XDG base
    // dirs: an `[env]` entry that spells one of them parses and appears
    // in the argv, but the later infrastructure emit wins — the payload
    // sees only the derived-from-HOME value.
    let mut cfg = podman_base(true);
    cfg.env
        .insert("XDG_CACHE_HOME".into(), "/synth/leak".into());
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let hits: Vec<String> = argv
        .windows(2)
        .filter(|w| w[0] == "--env" && w[1].starts_with("XDG_CACHE_HOME="))
        .map(|w| w[1].clone())
        .collect();
    assert_eq!(
        hits,
        vec![
            "XDG_CACHE_HOME=/synth/leak".to_string(),
            "XDG_CACHE_HOME=/mysbx-home/.cache".to_string()
        ],
        "the config entry must parse first and lose last: {hits:?}"
    );
}

#[test]
fn podman_backend_env_pins_win_over_the_config_layers() {
    // MYSBX_GVISOR_ENV carries environment that is only correct under
    // this backend (the container-side URL of the host's LiteLLM
    // forwarder): emitted after the layers, so a pin beats an `[env]`
    // entry of the same name.
    let mut cfg = podman_base(true);
    cfg.env
        .insert("MODEL_URL".into(), "http://127.0.0.1:4000/v1".into());
    let pins = ["MODEL_URL=http://192.168.84.1:14000/v1".to_string()];
    let mut params = podman_params();
    params.extra_env = &pins;
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    let hits: Vec<String> = argv
        .windows(2)
        .filter(|w| w[0] == "--env" && w[1].starts_with("MODEL_URL="))
        .map(|w| w[1].clone())
        .collect();
    assert_eq!(
        hits,
        vec![
            "MODEL_URL=http://127.0.0.1:4000/v1".to_string(),
            "MODEL_URL=http://192.168.84.1:14000/v1".to_string()
        ],
        "the config entry must come first and the pin last: {hits:?}"
    );
}

#[test]
fn podman_backend_env_pins_cannot_repoint_the_infrastructure_variables() {
    // The pins are trusted (they come from the Nix wrapper) but they
    // are still emitted BEFORE `HOME`/`PATH`/the XDG dirs, so the
    // sandbox invariants of config.md D14 stay the last word.
    let pins = [
        "HOME=/synth/leak".to_string(),
        "PATH=/synth/leak/bin".to_string(),
    ];
    let mut params = podman_params();
    params.extra_env = &pins;
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    for (var, want) in [
        ("HOME=", "HOME=/mysbx-home"),
        ("PATH=", "PATH=/bin:/usr/bin:/synth-image-tools"),
    ] {
        let last = argv
            .windows(2)
            .filter(|w| w[0] == "--env" && w[1].starts_with(var))
            .map(|w| w[1].clone())
            .next_back()
            .unwrap_or_else(|| panic!("no {var} in the argv: {argv:?}"));
        assert_eq!(last, want, "the infrastructure emit must win: {argv:?}");
    }
}

#[test]
fn podman_golden_state_dirs() {
    let mut cfg = podman_base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    cfg.state_dirs.push(".local/state/opencode".to_string());
    cfg.mounts.push(make_mount(
        "/synth/data/configs",
        Some("/inside/x"),
        Mode::Ro,
    ));
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    assert_golden("podman-state-dirs.txt", &argv);
}

#[test]
fn podman_golden_env_entry() {
    let mut cfg = podman_base(true);
    cfg.env.insert("EDITOR".into(), "repo-nvim".into());
    cfg.env.insert("PROJECT".into(), "demo".into());
    let host = host_env(&[
        ("TERM", "xterm-256color"),
        ("COLORTERM", "truecolor"),
        ("LANG", "C.UTF-8"),
        ("EDITOR", "host-nvim"),
    ]);
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host,
        &podman_params(),
    )
    .unwrap();
    assert_golden("podman-env-entry.txt", &argv);
}

#[test]
fn podman_golden_clone_session() {
    let cfg = podman_base(true);
    let mut repo = synth_repo();
    repo.worktrees = None; // Clone mode doesn't bind worktrees
    let mut params = podman_params();
    params.workspace = Workspace::Clone {
        clone: Path::new("/synth/repo.mysbx/clones/test"),
    };
    let argv = podman_run_argv(&cfg, &repo, &Payload::Shell, &host_env(&[]), &params).unwrap();
    assert_golden("podman-clone-session.txt", &argv);
}

#[test]
fn podman_no_run_no_host_store_paths_in_payload() {
    // bd myconfig-wao: the payload shell and tool PATH are image
    // paths, and the argv must carry no host `/nix/store` path
    // outside the mount sources — the container mounts nothing from
    // the host store, so a store path in the payload would die with
    // `no such file or directory` on the first run.
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();

    // The payload is the last argv entry: the shell — and there is
    // deliberately NO `--` separator before it: docker strips one
    // after the image, podman passes it through as the command's
    // argv[0] and runsc fails with `error finding executable "--"`
    // (bd myconfig-ivp).
    assert_eq!(argv[argv.len() - 1], "/bin/synth-shell");
    assert!(
        !argv.iter().any(|a| a == "--"),
        "no `--` separator in a podman argv: {argv:?}"
    );

    // No CA-bundle store pins: the image carries its own bundle in
    // its OCI env.
    for a in &argv {
        assert!(!a.contains("/nix/store"), "host store path in argv: {a}");
    }
}

#[test]
fn podman_container_name_is_unique_per_repo_path() {
    // The container name must differ between two repos that share a
    // basename — `--replace` would otherwise silently kill the
    // sibling session's container (bd myconfig-wao).
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let name = argv
        .iter()
        .zip(argv.iter().skip(1))
        .find(|(a, _)| a.as_str() == "--name")
        .map(|(_, n)| n.as_str())
        .expect("argv must carry --name");
    assert_eq!(
        name, "mysbx-repo-6e89dfc8f9",
        "basename plus path hash, got {name:?}"
    );
    // A repo with the SAME basename but a different path gets a
    // DIFFERENT container name.
    let sibling = Repo {
        root: PathBuf::from("/synth/other/repo"),
        sidecar: PathBuf::from("/synth/other/repo.mysbx"),
        git_dirs: Vec::new(),
        worktrees: None,
    };
    let argv2 = podman_run_argv(
        &podman_base(true),
        &sibling,
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let name2 = argv2
        .iter()
        .zip(argv2.iter().skip(1))
        .find(|(a, _)| a.as_str() == "--name")
        .map(|(_, n)| n.as_str())
        .expect("argv must carry --name");
    assert_eq!(name2, "mysbx-repo-d133a1ae6c");
    assert_ne!(name, name2, "same basename, different path: must differ");
}

#[test]
fn podman_no_run_no_host_home_beyond_declared_mounts() {
    let mut cfg = podman_base(true);
    cfg.mounts
        .push(make_mount("/synth/data/refs", None, Mode::Ro));
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();

    // Check that no host home paths appear
    let text = argv.join(" ");
    assert!(!text.contains("/home/"), "no host home path: {text}");
    assert!(!text.contains("$HOME"), "no literal $HOME: {text}");

    // Verify structure: only declared mounts and implicit binds
    let mount_indices: Vec<usize> = argv
        .iter()
        .enumerate()
        .filter(|(_, a)| a.as_str() == "--mount")
        .map(|(i, _)| i)
        .collect();

    // Extract all mount destinations
    let mount_dests: Vec<&str> = mount_indices
        .iter()
        .filter_map(|&i| {
            if i + 1 < argv.len() {
                argv.get(i + 1).map(|s| s.as_str())
            } else {
                None
            }
        })
        .collect();

    // Verify protected paths are not mount destinations
    for dest in &mount_dests {
        assert!(
            !dest.starts_with("/home/"),
            "mount destination {dest} should not be under /home/"
        );
        assert_ne!(*dest, "/", "root should not be a mount destination");
    }
}

#[test]
fn podman_mount_order_is_preserved() {
    let cfg = Merged {
        backend: Some("podman-gvisor".into()),
        network: true,
        mounts: vec![
            make_mount("/synth/data/outer", None, Mode::Ro),
            make_mount("/synth/other", None, Mode::Rw),
        ],
        env: BTreeMap::new(),
        git_dirs: Vec::new(),
        state_dirs: Vec::new(),
        forward_env: Vec::new(),
        allow_domains: Vec::new(),
        connect_ports: Vec::new(),
        listen_ports: Vec::new(),
        multiplexer: Multiplexer::None,
        display: Display::Off,
    };
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    // Check that mounts appear in order (podman uses --mount with combined strings)
    let outer = argv
        .iter()
        .position(|x| x.contains("src=/synth/data/outer"))
        .expect("mount for /synth/data/outer not found in argv");
    let nested = argv
        .iter()
        .position(|x| x.contains("src=/synth/other"))
        .expect("mount for /synth/other not found in argv");
    assert!(
        outer < nested,
        "the earlier declared mount must be bound first (outer at {outer}, nested at {nested})"
    );
}

/// The `--mount` specs of an argv, in order: every `--mount` flag with
/// the argument that follows it — the unit the tmpfs-ordering tests
/// below speak in (podman mounts are one flag + one spec string).
fn podman_mount_specs(argv: &[String]) -> Vec<&str> {
    argv.windows(2)
        .filter(|w| w[0] == "--mount")
        .map(|w| w[1].as_str())
        .collect()
}

#[test]
fn podman_home_is_a_fresh_tmpfs_emitted_before_every_bind() {
    // bd myconfig-lpl: the container root is read-only and podman's
    // bind copy-up materializes only the first path component, so a
    // home that exists only where a bind lands is not a home — the
    // payload shell died on its first XDG state write. The fix is the
    // bwrap shape (SANDBOX_HOME, config.md D14): a fresh empty tmpfs
    // at the container home, emitted BEFORE every bind so the binds
    // land ON TOP of it (the ro `~/.config/fish` seed mount and the
    // state-dirs stores included).
    //
    // The ordering is not just argv convention: BOTH engines under
    // this backend sort mounts parents-before-children regardless of
    // flag order — podman 5.8.x `libpod/util.go::sortMounts` ("Mounts
    // need to be sorted so paths will not cover other paths"), runsc
    // `runsc/boot/vfs.go::prepareMounts` ("Sort the mounts so that we
    // don't place children before parents") — so the tmpfs wins the
    // `/mysbx-home` mountpoint and every configured dest below it
    // wins over the tmpfs. The argv still emits the tmpfs FIRST: the
    // guarantee is relied on, not required.
    let mut cfg = podman_base(true);
    cfg.mounts.push(make_mount(
        "/home/synth/.config/fish",
        Some("/mysbx-home/.config"),
        Mode::Ro,
    ));
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let mounts = podman_mount_specs(&argv);

    // Exactly THREE tmpfs mounts — the home plus the two XDG
    // `.local` parents (bd myconfig-e50: runsc creates bind-mountpoint
    // parents root-owned, so the container user cannot create
    // siblings under a `.local/share` that exists only to host a
    // state bind). Podman mounts `/dev`, `/tmp` & co. itself
    // (`--read-only-tmpfs=true`); the user mounts mysbx adds are
    // binds, plus these tmpfs mounts.
    let tmpfs: Vec<&&str> = mounts
        .iter()
        .filter(|m| m.starts_with("type=tmpfs,"))
        .collect();
    assert_eq!(
        tmpfs,
        vec![
            &"type=tmpfs,dst=/mysbx-home",
            &"type=tmpfs,dst=/mysbx-home/.local/share",
            &"type=tmpfs,dst=/mysbx-home/.local/state",
        ],
        "home tmpfs plus the two XDG parents, nothing else: {mounts:?}"
    );

    // It comes FIRST among the mounts — before the repo bind and
    // before every configured bind.
    assert_eq!(
        mounts[0], "type=tmpfs,dst=/mysbx-home",
        "the home tmpfs precedes all binds: {mounts:?}"
    );
    assert!(
        mounts
            .iter()
            .any(|m| *m == "type=bind,src=/synth/repo,dst=/synth/repo,rw"),
        "repo bind present: {mounts:?}"
    );
    assert!(
        mounts
            .iter()
            .any(|m| *m == "type=bind,src=/home/synth/.config/fish,dst=/mysbx-home/.config,ro"),
        "ro config seed lands on top of the tmpfs: {mounts:?}"
    );
}

#[test]
fn podman_state_dirs_bind_over_the_home_tmpfs_not_the_ro_root() {
    // The D15 contract under this backend: a `state-dirs` entry is a
    // sidecar-backed rw bind at `/mysbx-home/<entry>` — which only
    // works because the section-4 tmpfs sits UNDER it (bd
    // myconfig-lpl). The argv must therefore express, in this order:
    // the home tmpfs, then the workspace bind, then the state binds,
    // then the configured mounts — the same layering bwrap gets from
    // its section-3 base binds.
    let mut cfg = podman_base(true);
    cfg.state_dirs.push(".local/state/opencode".to_string());
    cfg.mounts.push(make_mount(
        "/home/synth/.config/fish",
        Some("/mysbx-home/.config"),
        Mode::Ro,
    ));
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let mounts = podman_mount_specs(&argv);
    let pos = |needle: &str| {
        mounts
            .iter()
            .position(|m| m.contains(needle))
            .unwrap_or_else(|| panic!("{needle} not in {mounts:?}"))
    };
    let tmpfs = pos("type=tmpfs,dst=/mysbx-home");
    let repo = pos("src=/synth/repo,dst=/synth/repo");
    let state = pos("dst=/mysbx-home/.local/state/opencode");
    let config = pos("dst=/mysbx-home/.config");
    assert!(
        tmpfs < repo && repo < state && state < config,
        "tmpfs -> repo -> state bind -> configured mount, got {mounts:?}"
    );

    // `XDG_STATE_HOME` names a path that is now WRITABLE (the tmpfs
    // at the home backs `/mysbx-home/.local`, the state bind seeds
    // it) — the variable itself still points inside the home.
    let env: Vec<&str> = argv
        .windows(2)
        .filter(|w| w[0] == "--env")
        .map(|w| w[1].as_str())
        .collect();
    assert!(
        env.contains(&"XDG_STATE_HOME=/mysbx-home/.local/state"),
        "state home still derives from the container home: {env:?}"
    );
}

#[test]
fn podman_xdg_parent_tmpfs_precede_the_state_binds() {
    // bd myconfig-e50: runsc's mount preparation CREATES the missing
    // mountpoint dirs of a bind root-owned 0755, so a `.local/share`
    // that exists on the home tmpfs only to host the
    // `.local/share/opencode` state bind is not creatable-in for the
    // container user (`--userns=keep-id`) and fish died with EACCES
    // on `$XDG_DATA_HOME/fish`. The fix: tmpfs mounts at the two XDG
    // `.local` parents, emitted in section 4 — BEFORE the section-5a
    // state binds — so the argv reads parents-first and the state
    // binds land ON TOP of them (persistence unchanged: the D15
    // store wins the deeper dest in both engines' depth sorts).
    let mut cfg = podman_base(true);
    cfg.state_dirs.push(".local/share/opencode".to_string());
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let mounts = podman_mount_specs(&argv);
    let home_tmpfs = mounts
        .iter()
        .position(|m| *m == "type=tmpfs,dst=/mysbx-home")
        .expect("home tmpfs");
    let share_tmpfs = mounts
        .iter()
        .position(|m| *m == "type=tmpfs,dst=/mysbx-home/.local/share")
        .expect(".local/share tmpfs");
    let state_tmpfs = mounts
        .iter()
        .position(|m| *m == "type=tmpfs,dst=/mysbx-home/.local/state")
        .expect(".local/state tmpfs");
    let share_bind = mounts
        .iter()
        .position(|m| *m == "type=bind,src=/synth/repo.mysbx/state/.local/share/opencode,dst=/mysbx-home/.local/share/opencode,rw")
        .expect("share state bind");
    assert!(
        home_tmpfs < share_tmpfs && share_tmpfs < share_bind,
        "tmpfs(/mysbx-home) < tmpfs(/mysbx-home/.local/share) < \
         bind(.local/share/opencode), got {mounts:?}"
    );
    // `.local/state` gets its tmpfs too, unconditionally — even with
    // no state entry under it (nothing pre-exists in the image there,
    // an empty tmpfs dir is harmless).
    assert!(
        state_tmpfs < share_bind,
        "the .local/state tmpfs also precedes the state binds: {mounts:?}"
    );
}

#[test]
fn podman_state_dir_naming_an_xdg_parent_binds_over_the_parent_tmpfs() {
    // A `state-dirs` entry that EXACTLY names one of the section-4
    // XDG parent tmpfs dests coexists with the tmpfs at the same
    // dest: podman's sortMounts is a STABLE sort by destination
    // depth, so equal-depth user mounts keep argv order and the LATER
    // bind (section 5a, emitted after section 4) wins the mountpoint.
    // The entry's sidecar backing store therefore stays the visible,
    // persistent surface — the same semantics as without the tmpfs
    // (bd myconfig-e50).
    let mut cfg = podman_base(true);
    cfg.state_dirs.push(".local/share".to_string());
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let mounts = podman_mount_specs(&argv);
    let tmpfs = mounts
        .iter()
        .position(|m| *m == "type=tmpfs,dst=/mysbx-home/.local/share")
        .expect(".local/share tmpfs");
    let bind = mounts
        .iter()
        .position(|m| {
            *m == "type=bind,src=/synth/repo.mysbx/state/.local/share,dst=/mysbx-home/.local/share,rw"
        })
        .expect("the exact-name state bind");
    assert!(
        tmpfs < bind,
        "the state bind must follow the tmpfs in argv order (stable \
         same-depth sort makes the later bind win): {mounts:?}"
    );
    // And the unconditional sibling parent tmpfs is still there too.
    assert!(
        mounts
            .iter()
            .any(|m| *m == "type=tmpfs,dst=/mysbx-home/.local/state"),
        "the .local/state parent tmpfs stays: {mounts:?}"
    );
}

#[test]
fn podman_state_parent_dirs_are_tmpfsed_for_the_payload() {
    // bd myconfig-ixz, the e50 failure generalized: runsc creates the
    // missing bind-mountpoint ancestors root-owned 0755, so the
    // parent of a `state-dirs` entry — `.pi/agent`, which hosts the
    // `.pi/agent/sessions` state bind — is not writable for the
    // container user (`--userns=keep-id`), and pi died writing its
    // runtime files as SIBLINGS of the state dir
    // (`auth.json`/`settings.json`/`trust.json`/`models.json` DIRECTLY
    // into `.pi/agent`, all inside `getAgentDir()` — no XDG dirs).
    // The rule: state dirs are "the writable part of the home", so
    // their PARENT gets a tmpfs too — section 4, before the state
    // bind of section 5a.
    let mut cfg = podman_base(true);
    cfg.state_dirs.push(".pi/agent/sessions".to_string());
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let mounts = podman_mount_specs(&argv);
    let agent_tmpfs = mounts
        .iter()
        .position(|m| *m == "type=tmpfs,dst=/mysbx-home/.pi/agent")
        .expect("tmpfs at the state entry's parent .pi/agent");
    let sessions_bind = mounts
        .iter()
        .position(|m| *m == "type=bind,src=/synth/repo.mysbx/state/.pi/agent/sessions,dst=/mysbx-home/.pi/agent/sessions,rw")
        .expect("the .pi/agent/sessions state bind");
    assert!(
        agent_tmpfs < sessions_bind,
        "the .pi/agent tmpfs precedes the .pi/agent/sessions bind: {mounts:?}"
    );
    // ONLY the parent — the shallower ancestor `.pi` stays on the
    // home tmpfs (runsc may root-own it as the tmpfs's own
    // mountpoint ancestor, but the payload only needs x/search
    // through it — 0755 has that — its +w lands on `.pi/agent`
    // itself) — and `.config` is not part of the rule at all.
    assert!(
        !mounts
            .iter()
            .any(|m| *m == "type=tmpfs,dst=/mysbx-home/.pi"),
        "no tmpfs above the state entry's parent: {mounts:?}"
    );
    assert!(
        !mounts
            .iter()
            .any(|m| *m == "type=tmpfs,dst=/mysbx-home/.config"),
        "the ro config surface stays tmpfs-free: {mounts:?}"
    );
    // The e50 XDG parent tmpfses stay after the generalization.
    assert!(
        mounts
            .iter()
            .any(|m| *m == "type=tmpfs,dst=/mysbx-home/.local/share"),
        "the .local/share parent tmpfs stays: {mounts:?}"
    );
    assert!(
        mounts
            .iter()
            .any(|m| *m == "type=tmpfs,dst=/mysbx-home/.local/state"),
        "the .local/state parent tmpfs stays: {mounts:?}"
    );
}

#[test]
fn podman_state_parent_tmpfses_dedupe_entries_sharing_a_parent() {
    // Two entries under one parent ask for the same tmpfs dest once
    // (the argv must stay free of duplicate `--mount` dests); a
    // clone run drops every state BIND but keeps the parent tmpfs
    // (like the unconditional e50 pair: an empty tmpfs dir is
    // harmless — nothing pre-exists in the image at any of these
    // paths). Duplicate state entries are dropped by the merge
    // (config.md D15: first occurrence wins).
    let mut cfg = podman_base(true);
    cfg.state_dirs.push(".pi/agent/sessions".to_string());
    cfg.state_dirs.push(".pi/agent/mcp".to_string());
    cfg.state_dirs.push(".local/share/opencode".to_string());
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let mounts = podman_mount_specs(&argv);
    let agent = mounts
        .iter()
        .filter(|m| **m == "type=tmpfs,dst=/mysbx-home/.pi/agent")
        .count();
    assert_eq!(agent, 1, "one tmpfs per distinct parent: {mounts:?}");
    assert!(
        !mounts
            .iter()
            .any(|m| *m == "type=tmpfs,dst=/mysbx-home/.local/share/opencode"),
        "no tmpfs AT a state entry dest: {mounts:?}"
    );

    let mut params = podman_params();
    params.workspace = Workspace::Clone {
        clone: Path::new("/synth/repo.mysbx/clones/test"),
    };
    let argv = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    let mounts = podman_mount_specs(&argv);
    assert!(
        mounts
            .iter()
            .any(|m| *m == "type=tmpfs,dst=/mysbx-home/.pi/agent"),
        "a clone run keeps the state-parent tmpfs: {mounts:?}"
    );
    assert!(
        !mounts
            .iter()
            .any(|m| m.contains("src=/synth/repo.mysbx/state")),
        "a clone run mounts no state bind: {mounts:?}"
    );
}

#[test]
fn podman_mount_dest_on_the_container_home_is_refused() {
    // The one-directional home guard of bwrap (review-2 item 5), now
    // that the home is a tmpfs HERE too: a mount dest EQUAL to the
    // container home would replace the section-4 tmpfs while `HOME`
    // still names it, seeding the home with content no layer declared.
    // Descendants stay allowed — the fish-config seed mount proves it.
    let mut cfg = podman_base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some(CONTAINER_HOME), Mode::Rw));
    let err = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(
            err,
            mysbx::podman_gvisor::Error::ProtectedDest {
                protected: CONTAINER_HOME,
                ..
            }
        ),
        "wrong error: {err}"
    );
}

#[test]
fn podman_nested_state_dirs_are_refused() {
    let mut cfg = podman_base(true);
    cfg.state_dirs.push(".local/share".to_string());
    cfg.state_dirs.push(".local/share/opencode".to_string());
    let err = podman_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .expect_err("must be refused");
    assert!(
        matches!(err, mysbx::podman_gvisor::Error::StateDirNesting { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn podman_golden_with_resource_limits() {
    let mut params = podman_params();
    params.pids_limit = Some(Cow::from("100"));
    params.memory = Some(Cow::from("2g"));
    params.cpus = Some(Cow::from("1.5"));
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    assert_golden("podman-resource-limits.txt", &argv);
}

#[test]
fn podman_cgroups_ignored_skips_limits() {
    // When ignore_cgroups=true, resource limits should NOT be applied
    let mut params = podman_params();
    params.pids_limit = Some(Cow::from("100"));
    params.memory = Some(Cow::from("2g"));
    params.cpus = Some(Cow::from("1.5"));
    params.ignore_cgroups = true;

    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();

    // Verify resource limit flags are NOT present
    assert!(
        !argv.iter().any(|a| a == "--pids-limit"),
        "--pids-limit should not appear when cgroups are ignored"
    );
    assert!(
        !argv.iter().any(|a| a == "--memory"),
        "--memory should not appear when cgroups are ignored"
    );
    assert!(
        !argv.iter().any(|a| a == "--cpus"),
        "--cpus should not appear when cgroups are ignored"
    );
}

#[test]
fn podman_with_runtime_flags() {
    let mut params = podman_params();
    let runtime_flags = vec![
        "--log-level=debug".to_string(),
        "ignore-cgroups".to_string(),
    ];
    params.runtime_flags = &runtime_flags;

    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();

    // Runtime flags should appear in argv (exact placement depends on implementation)
    // For now, just verify they're present somewhere
    let text = argv.join(" ");
    assert!(
        text.contains("--log-level=debug"),
        "runtime flags should be in argv: {text}"
    );
}

#[test]
fn podman_cgroup_manager_omitted_when_none() {
    // None ⇒ no --cgroup-manager flag at all: the gvisor tier's root
    // shape (the system manager owns the hierarchy). A hardcoded
    // cgroupfs made runsc try to write a cgroup it cannot and fail
    // with "cannot set up cgroup for root" (bd myconfig-b13).
    let mut params = podman_params();
    params.cgroup_manager = None;

    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();

    assert!(
        !argv.iter().any(|a| a.starts_with("--cgroup-manager")),
        "--cgroup-manager should be omitted when params say None: {argv:?}"
    );
    assert_eq!(argv[0], "--runtime=runsc");
}

#[test]
fn podman_attached_run_wires_stdio() {
    // bd myconfig-jho: without `--interactive` podman closes the
    // container's stdin, an interactive shell payload reads instant
    // EOF and exits 0 — the f13 "exits immediately, no container, no
    // error" failure. The argv must attach the container to mysbx's
    // own stdio: `--interactive` always, `--tty` when the operator is
    // on a terminal (so a piped one-shot is not forced onto a pty).
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &podman_params(),
    )
    .unwrap();
    let run_at = argv.iter().position(|a| a == "run").expect("`run` in argv");
    assert!(
        argv.contains(&"--interactive".to_string()),
        "--interactive must wire the container's stdin: {argv:?}"
    );
    assert!(
        argv[..run_at + 3].contains(&"--interactive".to_string()),
        "--interactive belongs to the run flags right after `run`: {argv:?}"
    );
    assert!(
        !argv.contains(&"--tty".to_string()),
        "no --tty without a terminal: {argv:?}"
    );

    let mut params = podman_params();
    params.tty = true;
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    assert!(
        argv.contains(&"--tty".to_string()),
        "--tty must follow --interactive on a terminal: {argv:?}"
    );
    assert!(
        argv.iter().position(|a| a == "--interactive") < argv.iter().position(|a| a == "--tty"),
        "--interactive precedes --tty: {argv:?}"
    );

    // A run that attaches nothing is possible for future callers —
    // but the flags are opt-OUT per params, never silently omitted.
    let mut params = podman_params();
    params.interactive = false;
    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    assert!(
        !argv.contains(&"--interactive".to_string()),
        "opt-out drops the flag: {argv:?}"
    );
}

#[test]
fn podman_rootless_defaults_golden() {
    // The argv of a real rootless run: lib.rs defaults euid != 0 to
    // cgroupfs + ignore-cgroups (the gvisor tier's rootless defaults),
    // so runsc never touches the cgroup hierarchy. Byte-for-byte
    // against a golden — the flags are the fix for bd myconfig-b13,
    // and a regression here is exactly the f13 failure.
    let runtime_flags = ["ignore-cgroups".to_string()];
    let params = PodmanParams {
        runtime_flags: &runtime_flags,
        cgroup_manager: Some("cgroupfs"),
        ..podman_params()
    };

    let argv = podman_run_argv(
        &podman_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .unwrap();
    assert_golden("podman-rootless-defaults.txt", &argv);
}

// ---- nono backend tests ------------------------------------------------------

/// A synthetic nono params: HOST pins like bwrap's (nono runs the
/// payload on the host kernel), the `default` profile lib.rs falls
/// back to.
fn nono_params() -> NonoParams<'static> {
    NonoParams {
        shell: "/synth/bin/bash",
        tools_path: "/synth/bin",
        policy_paths: &[],
        workspace: Workspace::Live,
        profile: "default",
    }
}

/// A nono base config: no allowlist entries — callers that test the
/// network mapping fill them.
fn nono_base(network: bool) -> Merged {
    Merged {
        backend: Some("nono".into()),
        network,
        mounts: Vec::new(),
        env: BTreeMap::new(),
        git_dirs: Vec::new(),
        state_dirs: Vec::new(),
        forward_env: Vec::new(),
        allow_domains: Vec::new(),
        connect_ports: Vec::new(),
        listen_ports: Vec::new(),
        multiplexer: Multiplexer::None,
        display: Display::Off,
    }
}

#[test]
fn nono_golden_minimal() {
    // network = false: `--block-net`, no `--allow-unix-socket` (the
    // daemon socket is a network service under a denied network).
    let argv = nono_run_argv(
        &nono_base(false),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .unwrap();
    assert_golden("nono-minimal.txt", &argv);
    assert!(
        !argv.contains(&"--allow-unix-socket".to_string()),
        "no daemon socket under a denied network: {argv:?}"
    );
}

#[test]
fn nono_golden_allowlist() {
    // The core mapping of bd myconfig-6di.2: the allowlist becomes
    // `--allow-domain` per domain in order, `--allow-connect-port` per
    // port, and — with a shared network — the daemon socket flag. No
    // `--block-net`, no `--listen-port` (none configured).
    let mut cfg = nono_base(true);
    cfg.allow_domains = vec!["api.openai.com".into(), "github.com".into()];
    cfg.connect_ports = vec![443];
    let argv = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .unwrap();
    assert_golden("nono-allowlist.txt", &argv);
    let allow_domain_at: Vec<usize> = argv
        .windows(2)
        .filter(|w| w[0] == "--allow-domain")
        .map(|w| argv.iter().position(|a| *a == w[1]).unwrap())
        .collect();
    let domains: Vec<&str> = argv
        .windows(2)
        .filter(|w| w[0] == "--allow-domain")
        .map(|w| w[1].as_str())
        .collect();
    assert_eq!(domains, ["api.openai.com", "github.com"]);
    assert_eq!(allow_domain_at.len(), 2);
    assert!(
        argv.windows(2)
            .any(|w| w[0] == "--allow-unix-socket" && w[1] == "/nix/var/nix/daemon-socket/socket"),
        "the daemon socket is granted with the shared network: {argv:?}"
    );
    assert!(!argv.contains(&"--block-net".to_string()));
    assert!(
        !argv.windows(2).any(|w| w[0] == "--listen-port"),
        "no listen-port flag without a configured listen port: {argv:?}"
    );
}

#[test]
fn nono_golden_ro_mount() {
    // A read-only mount is a `--read` grant at its own path.
    let mut cfg = nono_base(false);
    cfg.mounts
        .push(make_mount("/synth/data/refs", None, Mode::Ro));
    let argv = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .unwrap();
    assert_golden("nono-ro-mount.txt", &argv);
    assert!(
        argv.windows(2)
            .any(|w| w[0] == "--read" && w[1] == "/synth/data/refs"),
        "ro mount is a --read: {argv:?}"
    );
}

#[test]
fn nono_golden_rw_mount() {
    // A read-write mount is an `--allow` grant at its own path.
    let mut cfg = nono_base(false);
    cfg.mounts
        .push(make_mount("/synth/data/cache", None, Mode::Rw));
    let argv = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .unwrap();
    assert_golden("nono-rw-mount.txt", &argv);
    assert!(
        argv.windows(2)
            .any(|w| w[0] == "--allow" && w[1] == "/synth/data/cache"),
        "rw mount is an --allow: {argv:?}"
    );
}

#[test]
fn nono_golden_state_dirs() {
    // state-dirs land at their REAL sidecar path — the semantic
    // difference from the other backends: no remap, so the payload
    // sees `<repo>.mysbx/state/<entry>`.
    let mut cfg = nono_base(false);
    cfg.state_dirs = vec![".local/share/opencode".into(), ".cache/foo".into()];
    let argv = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .unwrap();
    assert_golden("nono-state-dirs.txt", &argv);
    for entry in [
        "/synth/repo.mysbx/state/.local/share/opencode",
        "/synth/repo.mysbx/state/.cache/foo",
    ] {
        assert!(
            argv.windows(2).any(|w| w[0] == "--allow" && w[1] == entry),
            "state store {entry} missing from the argv: {argv:?}"
        );
    }
}

#[test]
fn nono_golden_command_payload() {
    // A one-shot: the payload verbatim after `--`, the shell pin never
    // entered the argv.
    let argv = nono_run_argv(
        &nono_base(false),
        &synth_repo(),
        &Payload::Command(vec!["ls".into(), "-la".into()]),
        &host_env(&[]),
        &nono_params(),
    )
    .unwrap();
    assert_golden("nono-command.txt", &argv);
    assert!(
        !argv.contains(&"/synth/bin/bash".to_string()),
        "the shell pin is not the one-shot payload: {argv:?}"
    );
}

#[test]
fn nono_golden_git_dirs() {
    // The approved git dir is `--allow`ed like the repo root.
    let mut cfg = nono_base(false);
    cfg.git_dirs = vec![PathBuf::from("/synth/gitdirs/main")];
    let argv = nono_run_argv(
        &cfg,
        &worktree_repo(&["/synth/gitdirs/main"]),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .unwrap();
    assert_golden("nono-git-dirs.txt", &argv);
    assert!(
        argv.windows(2)
            .any(|w| w[0] == "--allow" && w[1] == "/synth/gitdirs/main"),
        "the approved git dir is granted: {argv:?}"
    );
}

#[test]
fn nono_refuses_a_dest_remap() {
    // Landlock grants access AT a path, it cannot move one: a `dest`
    // different from the source is refused, `dest == path` is fine
    // (it is no remap).
    let mut cfg = nono_base(false);
    cfg.mounts
        .push(make_mount("/synth/data/refs", Some("/inside"), Mode::Ro));
    let err = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .expect_err("a remap must be refused");
    assert!(
        matches!(err, mysbx::nono::Error::RemapUnsupported { .. }),
        "wrong error: {err}"
    );

    let mut cfg = nono_base(false);
    cfg.mounts.push(make_mount(
        "/synth/data/refs",
        Some("/synth/data/refs"),
        Mode::Ro,
    ));
    let argv = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .expect("dest == path is no remap");
    assert!(
        argv.windows(2)
            .any(|w| w[0] == "--read" && w[1] == "/synth/data/refs"),
        "the dest==path mount is a plain read grant: {argv:?}"
    );
}

#[test]
fn nono_refuses_a_clone_run() {
    // The clone-remap (clone bound AT the repo path, workspace.md D3)
    // is inexpressible under Landlock.
    let workspace = Workspace::Clone {
        clone: Path::new("/synth/repo.mysbx/clones/s1"),
    };
    let params = NonoParams {
        workspace,
        ..nono_params()
    };
    let err = nono_run_argv(
        &nono_base(false),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .expect_err("a clone run must be refused");
    assert!(
        matches!(err, mysbx::nono::Error::CloneUnsupported),
        "wrong error: {err}"
    );
}

#[test]
fn nono_refuses_every_session_starting_multiplexer_for_the_shell_only() {
    // The interactive payload: a session-starting multiplexer is
    // refused (no tmpfs home for the private socket directory).
    for mux in [
        Multiplexer::Tmux,
        Multiplexer::Workmux,
        Multiplexer::Herdr,
        Multiplexer::Aoe,
        Multiplexer::Orca,
    ] {
        let mut cfg = nono_base(false);
        cfg.multiplexer = mux;
        let err = nono_run_argv(
            &cfg,
            &synth_repo(),
            &Payload::Shell,
            &host_env(&[]),
            &nono_params(),
        )
        .expect_err("a session multiplexer must be refused");
        assert!(
            matches!(err,
                mysbx::nono::Error::MultiplexerUnavailable { multiplexer }
                if multiplexer == mux),
            "wrong error: {err}"
        );
    }

    // A one-shot never starts a session, so the multiplexer does not
    // refuse it (cli.md D11).
    let mut cfg = nono_base(false);
    cfg.multiplexer = Multiplexer::Tmux;
    let argv = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Command(vec!["true".into()]),
        &host_env(&[]),
        &nono_params(),
    )
    .expect("a one-shot is not a session");
    assert_eq!(argv[argv.len() - 1], "true");
}

#[test]
fn nono_refuses_the_waypipe_display_for_both_payload_forms() {
    // The waypipe syscall set is unaudited under nono's seccomp filter
    // — refused for the shell AND for a one-shot.
    for payload in [Payload::Shell, Payload::Command(vec!["true".into()])] {
        let mut cfg = nono_base(false);
        cfg.display = Display::Waypipe;
        let err = nono_run_argv(
            &cfg,
            &synth_repo(),
            &payload,
            &host_env(&[]),
            &nono_params(),
        )
        .expect_err("waypipe must be refused");
        assert!(
            matches!(err, mysbx::nono::Error::DisplayUnavailable),
            "wrong error: {err}"
        );
    }
}

#[test]
fn nono_refuses_a_shared_network_without_an_allowlist() {
    // `network = true` (the mysbx default) with no allowlist: nono
    // cannot express "share the host network".
    let err = nono_run_argv(
        &nono_base(true),
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .expect_err("a shared network must be refused");
    assert!(
        matches!(err, mysbx::nono::Error::NetworkSharedUnsupported),
        "wrong error: {err}"
    );
}

#[test]
fn nono_refuses_an_allowlist_under_a_denied_network() {
    // `network = false` with a non-empty allowlist contradicts the
    // deny — defense in depth next to the pipeline's step 4b refusal.
    for (domains, ports, listen) in [
        (vec!["api.openai.com".to_string()], vec![], vec![]),
        (vec![], vec![443], vec![]),
        (vec![], vec![], vec![8080]),
    ] {
        let mut cfg = nono_base(false);
        cfg.allow_domains = domains;
        cfg.connect_ports = ports;
        cfg.listen_ports = listen;
        let err = nono_run_argv(
            &cfg,
            &synth_repo(),
            &Payload::Shell,
            &host_env(&[]),
            &nono_params(),
        )
        .expect_err("an allowlist under a denied network must be refused");
        assert!(
            matches!(err, mysbx::nono::Error::AllowlistUnderDeniedNetwork),
            "wrong error: {err}"
        );
    }
}

#[test]
fn nono_refuses_an_unapproved_git_dir() {
    // The repo's `.git` pointer names a directory no trusted layer
    // approved — the bwrap/podman rule (review-2 item 1).
    let err = nono_run_argv(
        &nono_base(false),
        &worktree_repo(&["/synth/gitdirs/main"]),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .expect_err("an unapproved git dir must be refused");
    assert!(
        matches!(err, mysbx::nono::Error::GitDirNotApproved { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn nono_refuses_nested_state_dirs() {
    // Two entries that nest: `--allow` is recursive, the inner entry
    // is redundant at best (config.md D15).
    let mut cfg = nono_base(false);
    cfg.state_dirs = vec!["a".into(), "a/b".into()];
    let err = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .expect_err("nested state dirs must be refused");
    assert!(
        matches!(err, mysbx::nono::Error::StateDirNesting { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn nono_refuses_a_writable_grant_over_a_policy_file() {
    // An `rw` mount that contains a guarded policy path exposes it —
    // the same refusal the other backends run (review-3 item 3).
    let policy = [mysbx::bwrap::PolicyPath::lexical(
        "/synth/home/.config/mysbx/config.toml",
    )];
    let params = NonoParams {
        policy_paths: &policy,
        ..nono_params()
    };
    let mut cfg = nono_base(false);
    cfg.mounts
        .push(make_mount("/synth/home/.config/mysbx", None, Mode::Rw));
    let err = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &params,
    )
    .expect_err("a writable policy dir must be refused");
    assert!(
        matches!(err, mysbx::nono::Error::PolicyFileWritable { .. }),
        "wrong error: {err}"
    );
}

#[test]
fn nono_refuses_a_grant_under_the_nix_daemon_dir_when_the_network_is_denied() {
    // `network = false`: a source at, below or containing
    // `/nix/var/nix` hands the daemon socket back — refused across
    // ALL mount sources, ro included (review-3 item 2's broadened
    // guard).
    for (mode, below) in [(Mode::Ro, true), (Mode::Rw, true), (Mode::Ro, false)] {
        let path = if below {
            "/nix/var/nix/daemon-socket"
        } else {
            "/nix"
        };
        let mut cfg = nono_base(false);
        cfg.mounts.push(make_mount(path, None, mode));
        let err = nono_run_argv(
            &cfg,
            &synth_repo(),
            &Payload::Shell,
            &host_env(&[]),
            &nono_params(),
        )
        .expect_err("a daemon-dir grant under a denied network must be refused");
        assert!(
            matches!(err, mysbx::nono::Error::DaemonUnderDeniedNetwork { .. }),
            "wrong error for {path} ({mode:?}): {err}"
        );
    }
}

#[test]
fn nono_refuses_a_writable_ancestor_of_a_state_store() {
    // An `rw` mount source containing a state backing store lets the
    // payload swap it for a symlink — refused (config.md D15).
    let mut cfg = nono_base(false);
    cfg.state_dirs = vec!["x".into()];
    cfg.mounts
        .push(make_mount("/synth/repo.mysbx", None, Mode::Rw));
    let err = nono_run_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env(&[]),
        &nono_params(),
    )
    .expect_err("a writable state tree must be refused");
    assert!(
        matches!(err, mysbx::nono::Error::StateTreeWritable { .. }),
        "wrong error: {err}"
    );
}
