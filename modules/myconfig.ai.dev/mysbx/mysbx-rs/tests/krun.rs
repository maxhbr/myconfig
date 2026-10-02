// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Golden argv tests of the direct-libkrun backend (`backend =
// "krun"`, docs/design/backends.md D3, bd myconfig-dak.3).
//!
//! Every fixture under `tests/assets/krun/*.txt` is the *expected*
//! launcher argv of [`krun_argv`] for one case, one argument per
//! line, compared byte for byte — the same convention as
//! `tests/argv.rs` and the podman goldens. The builder is pure
//! (paths are synthetic `/synth/...`), so the fixtures need no
//! tempdirs and are machine-independent.

use mysbx::bwrap::{Payload, Workspace};
use mysbx::config::{Mode, Mount, Multiplexer};
use mysbx::krun::{
    backing_paths, krun_argv, Params, DEFAULT_CPUS, DEFAULT_RAM_MIB, GUEST_SHARE_ROOT, SCRATCH_IMG,
};
use mysbx::merge::Merged;
use mysbx::repo::Repo;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::path::PathBuf;

/// A synthetic repo (paths need not exist — the builder is pure).
fn synth_repo() -> Repo {
    Repo {
        root: PathBuf::from("/home/synth/repo"),
        sidecar: PathBuf::from("/home/synth/repo.mysbx"),
        git_dirs: Vec::new(),
        worktrees: None,
    }
}

fn base(network: bool) -> Merged {
    Merged {
        backend: Some("krun".into()),
        network,
        mounts: Vec::new(),
        env: BTreeMap::new(),
        git_dirs: Vec::new(),
        state_dirs: Vec::new(),
        forward_env: Vec::new(),
        allow_domains: Vec::new(),
        connect_ports: Vec::new(),
        listen_ports: Vec::new(),
        memory: None,
        cpus: None,
        pids_limit: None,
        multiplexer: Multiplexer::None,
        display: mysbx::config::Display::Off,
    }
}

fn make_mount(path: &str, dest: Option<&str>, mode: Mode) -> Mount {
    Mount {
        file: false,
        path: path.to_string(),
        dest: dest.map(|d| d.to_string()),
        mode,
    }
}

fn params<'a>(
    rootfs: &'a str,
    shell: &'a str,
    mux_entry: Option<&'a str>,
    workspace: Workspace<'a>,
) -> Params<'a> {
    Params {
        rootfs,
        shell,
        tools_path: "/synth/bin",
        ca_bundle: None,
        mux_entry,
        workspace,
        memory: None,
        cpus: None,
        git_trust: None,
        scratch: None,
    }
}

fn assert_golden(rel: &str, argv: &[String]) {
    let path = format!("tests/assets/krun/{rel}");
    let mut rendered = String::new();
    for arg in argv {
        rendered.push_str(arg);
        rendered.push('\n');
    }
    // The same bless convention as tests/argv.rs:
    // MYSBX_BLESS_GOLDEN=1 rewrites the asset after a deliberate argv
    // change.
    if std::env::var_os("MYSBX_BLESS_GOLDEN").is_some() {
        std::fs::write(&path, &rendered).unwrap_or_else(|e| panic!("cannot bless {path}: {e}"));
        return;
    }
    let expected =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("cannot read golden {path}: {e}"));
    if rendered != expected {
        panic!("golden {rel} mismatch:\n--- expected ---\n{expected}\n--- actual ---\n{rendered}");
    }
}

fn run(cfg: &Merged, repo: &Repo, payload: &Payload, params: &Params<'_>) -> Vec<String> {
    // The resolver fixture pins the goldens — the host's real
    // /etc/resolv.conf differs per machine (bd myconfig-dak.6).
    std::env::set_var("MYSBX_RESOLV_SOURCE", "tests/assets/krun/resolv.conf");
    let host_env = BTreeMap::new();
    krun_argv(cfg, repo, payload, &host_env, params).expect("the argv builds")
}

#[test]
fn golden_minimal_config() {
    let argv = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert_golden("minimal.txt", &argv);
}

#[test]
fn golden_one_ro_mount() {
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/home/synth/data", None, Mode::Ro));
    let argv = run(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert_golden("ro-mount.txt", &argv);
}

#[test]
fn golden_one_rw_mount_with_explicit_dest() {
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/srv/data"), Mode::Rw));
    let argv = run(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert_golden("rw-mount.txt", &argv);
}

#[test]
fn golden_state_dirs() {
    let mut cfg = base(true);
    cfg.state_dirs.push(".cache".into());
    let argv = run(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert_golden("state-dirs.txt", &argv);
}

#[test]
fn golden_command_payload() {
    let argv = run(
        &base(true),
        &synth_repo(),
        &Payload::Command(vec!["/synth/bin/prog".into(), "--flag".into()]),
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert_golden("command.txt", &argv);
}

#[test]
fn golden_clone_session() {
    let argv = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params(
            "/synth/rootfs",
            "/synth/shell",
            None,
            Workspace::Clone {
                clone: Path::new("/synth/repo.mysbx/clones/s1"),
            },
        ),
    );
    assert_golden("clone-session.txt", &argv);
}

#[test]
fn backing_paths_map_every_share_to_its_staging_slot() {
    // bd myconfig-n4b: the trust texts must name the share BACKING
    // paths too (nix's libgit2 realpaths the symlinked workspace).
    // The map: the workspace at its repo path, a mount at its dest,
    // the store, and the trust files — each <GUEST_SHARE_ROOT>/
    // <stage-device>/<slot>.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/git", Some("/srv/gitdir"), Mode::Rw));
    let p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    let map = backing_paths(&cfg, &synth_repo(), &p).expect("the backing map");
    let lookup = |sandbox: &str| {
        map.iter()
            .find(|(s, _)| s == sandbox)
            .map(|(_, b)| b.clone())
            .unwrap_or_else(|| panic!("no backing path for {sandbox}"))
    };
    assert_eq!(lookup("/nix/store"), "/tmp/mysbx-shares/stage-ro/store");
    assert_eq!(
        lookup("/home/synth/repo"),
        "/tmp/mysbx-shares/stage-rw/workspace"
    );
    assert_eq!(
        lookup("/srv/gitdir"),
        "/tmp/mysbx-shares/stage-rw/m-7a9d67a26a"
    );
}

#[test]
fn a_scratch_param_renders_the_flag_and_the_announcement() {
    // The scratch disk (bd myconfig-dak.7): the flag names the
    // launcher-view SCRATCH_IMG, and the PRESENCE announcement rides
    // the manifest env (the init identifies the disk as the only
    // /dev/vd*, never by name). Without the param neither appears.
    let mut p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    p.scratch = Some(SCRATCH_IMG);
    let argv = run(&base(true), &synth_repo(), &Payload::Shell, &p);
    let i = argv
        .iter()
        .position(|a| a == "--scratch")
        .expect("the --scratch flag");
    assert_eq!(argv[i + 1], SCRATCH_IMG);
    let j = argv
        .windows(2)
        .position(|w| w[0] == "--env" && w[1] == "MYSBX_KRUN_SCRATCH=1")
        .expect("the manifest env announcement");
    // The announcement rides AFTER the network flag section, before
    // --chdir (render order: cpus/ram, rootfs, init, devices,
    // manifest, shares, envs, network, scratch, chdir, payload).
    assert!(argv[i + 1..j].iter().all(|a| a != "--chdir"));
    // Without the param: no flag, no announcement.
    let argv = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert!(!argv.iter().any(|a| a == "--scratch"));
    assert!(!argv.windows(2).any(|w| w[1] == "MYSBX_KRUN_SCRATCH=1"));
}

#[test]
fn clone_run_forces_configured_rw_mounts_read_only() {
    // workspace.md D4: in a clone run EVERY configured mount is
    // read-only — the session clone is the only writable bind.
    // Both halves of the backend's mount plumbing must carry the
    // downgrade: the staging binds (which tree a host dir lands in)
    // and the shares (the payload's contract).
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/srv/data"), Mode::Rw));
    let clone_path = Path::new("/synth/repo.mysbx/clones/s1");
    let p = params(
        "/synth/rootfs",
        "/synth/shell",
        None,
        Workspace::Clone { clone: clone_path },
    );
    let argv = run(&cfg, &synth_repo(), &Payload::Shell, &p);
    // The share rides the RO staging device, marked ro:
    assert!(argv
        .windows(4)
        .any(|w| w[0] == "--ro-share" && w[1].starts_with("stage-ro:m-")));
    assert!(!argv
        .windows(2)
        .any(|w| w[0] == "--rw-share" && w[1].starts_with("stage-rw:m-")));
    // The staging bind carries the same downgrade:
    let binds = mysbx::krun::stage_binds(&cfg, &synth_repo(), &p);
    let (bind, ro, slot) = binds
        .iter()
        .find(|(_, _, slot)| slot.starts_with("m-"))
        .expect("the configured mount's staging bind");
    assert_eq!(bind, "/synth/data");
    assert!(ro, "a configured rw mount is ro in a clone run");
    assert!(slot.starts_with("m-"));

    // The same config stays writable in a live run:
    let p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    let argv = run(&cfg, &synth_repo(), &Payload::Shell, &p);
    assert!(argv
        .windows(4)
        .any(|w| w[0] == "--rw-share" && w[1].starts_with("stage-rw:m-")));
    assert!(!argv
        .windows(2)
        .any(|w| w[0] == "--ro-share" && w[1].starts_with("stage-ro:m-")));
    let binds = mysbx::krun::stage_binds(&cfg, &synth_repo(), &p);
    let (_, ro, _) = binds
        .iter()
        .find(|(_, _, slot)| slot.starts_with("m-"))
        .expect("the configured mount's staging bind");
    assert!(!ro, "the same mount stays rw in a live run");
}

#[test]
fn golden_multiplexer_session() {
    let mut cfg = base(true);
    cfg.multiplexer = Multiplexer::Tmux;
    let argv = run(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &params(
            "/synth/rootfs",
            "/synth/shell",
            Some("/synth/entry/tmux"),
            Workspace::Live,
        ),
    );
    assert_golden("mux-session.txt", &argv);
}

#[test]
fn a_session_multiplexer_without_an_entry_is_refused() {
    let mut cfg = base(true);
    cfg.multiplexer = Multiplexer::Tmux;
    let host_env = BTreeMap::new();
    let err = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .unwrap_err();
    assert_eq!(
        err,
        mysbx::krun::Error::MultiplexerUnavailable {
            multiplexer: Multiplexer::Tmux,
        }
    );
    // A one-shot command never starts a session (cli.md D11): the
    // same config runs fine as `run -- CMD`.
    let argv = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Command(vec!["/synth/bin/true".into()]),
        &host_env,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert!(argv.is_ok());
}

#[test]
fn a_pids_limit_is_refused() {
    let mut cfg = base(true);
    cfg.pids_limit = Some(100);
    let host_env = BTreeMap::new();
    let err = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .unwrap_err();
    assert_eq!(err, mysbx::krun::Error::PidsLimit { pids: 100 });
}

#[test]
fn fractional_cpus_are_refused_not_rounded() {
    let mut cfg = base(true);
    cfg.cpus = Some("1.5".into());
    let host_env = BTreeMap::new();
    let mut p = params_owned("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    p.cpus = Some(Cow::from("1.5".to_owned()));
    let _ = &cfg;
    let err = krun_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env, &p).unwrap_err();
    assert!(matches!(
        err,
        mysbx::krun::Error::KrunLimit { key: "cpus", .. }
    ));
}

#[test]
fn limits_size_the_vm() {
    // memory/cpus override the defaults through the pin-shaped
    // params (lib.rs applies the env pins before the builder).
    let cfg = base(true);
    let host_env = BTreeMap::new();
    let mut p = params_owned("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    p.cpus = Some(Cow::from("4".to_owned()));
    p.memory = Some(Cow::from("8g".to_owned()));
    let argv = krun_argv(&cfg, &synth_repo(), &Payload::Shell, &host_env, &p).unwrap();
    assert_eq!(argv[argv.len() - 1], "/synth/shell");
    assert!(argv.windows(2).any(|w| w[0] == "--cpus" && w[1] == "4"));
    assert!(argv.windows(2).any(|w| w[0] == "--ram" && w[1] == "8192"));
    // The default sizes appear without any limit.
    let argv = run(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--cpus" && w[1] == DEFAULT_CPUS.to_string()));
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--ram" && w[1] == DEFAULT_RAM_MIB.to_string()));
}

#[test]
fn env_order_is_forwarded_then_config_then_infrastructure() {
    let mut cfg = base(true);
    cfg.env.insert("EDITOR".into(), "vi".into());
    let mut host_env = BTreeMap::new();
    host_env.insert("TERM".into(), "xterm-256color".into());
    let argv = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .unwrap();
    // The --env sequence (bwrap.rs section 6's precedence): TERM
    // (forwarded host var), EDITOR (config layer — wins over the
    // host by being set later), HOME + the XDG block + PATH (the
    // infrastructure block, LAST so no layer can repoint them).
    let envs: Vec<&str> = argv
        .windows(2)
        .filter(|w| w[0] == "--env")
        .map(|w| w[1].as_str())
        .collect();
    let term = envs.iter().position(|e| e.starts_with("TERM="));
    let editor = envs.iter().position(|e| e.starts_with("EDITOR="));
    let home = envs.iter().position(|e| e.starts_with("HOME="));
    let xdg = envs.iter().position(|e| e.starts_with("XDG_CONFIG_HOME="));
    let path = envs.iter().position(|e| e.starts_with("PATH="));
    let (Some(term), Some(editor), Some(home), Some(xdg), Some(path)) =
        (term, editor, home, xdg, path)
    else {
        panic!("missing env entries: {envs:?}");
    };
    assert!(
        term < editor && editor < home && home < xdg && xdg < path,
        "{envs:?}"
    );
}

#[test]
fn network_none_names_the_flag_shared_is_silent() {
    // bd myconfig-dak.6: `none` is the only mode the argv names —
    // shared is the launcher's default (the implicit vsock's TSI).
    let denied = run(
        &base(false),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    let i = denied
        .iter()
        .position(|a| a == "--network")
        .expect("the denied run names --network none");
    assert_eq!(denied[i + 1], "none");

    let shared = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert!(!shared.iter().any(|a| a == "--network"));
}

#[test]
fn the_store_share_is_first_and_ro_the_workspace_rw() {
    let argv = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    // Section 3 order: the staged devices (one per access mode),
    // the store share first (a ro slot like every other), workspace
    // rw second. The sandbox paths are the destinations the payload
    // contract promises; the guest mount root never appears in the
    // argv (the init derives it from the device tag).
    assert_eq!(argv[8], "--ro-device");
    assert_eq!(argv[9], "stage-ro=/mysbx-krun-stage/ro");
    assert_eq!(argv[10], "--rw-device");
    assert_eq!(argv[11], "stage-rw=/mysbx-krun-stage/rw");
    // The manifest pointer (the cmdline-budget finding): the
    // launcher assembles env/shares/chdir into this file instead of
    // the kernel cmdline.
    assert_eq!(argv[12], "--manifest");
    assert_eq!(argv[13], "/mysbx-krun-stage/ro/manifest");
    assert_eq!(argv[14], "--ro-share");
    assert_eq!(argv[15], "stage-ro:store@/nix/store ro");
    assert_eq!(argv[16], "--rw-share");
    assert_eq!(argv[17], "stage-rw:workspace@/home/synth/repo rw");
    assert!(!argv.iter().any(|a| a.contains(GUEST_SHARE_ROOT)));
}

fn params_owned<'a>(
    rootfs: &'a str,
    shell: &'a str,
    mux_entry: Option<&'a str>,
    workspace: Workspace<'a>,
) -> Params<'a> {
    Params {
        rootfs,
        shell,
        tools_path: "/synth/bin",
        ca_bundle: None,
        mux_entry,
        workspace,
        memory: None,
        cpus: None,
        git_trust: None,
        scratch: None,
    }
}

use std::path::Path;

#[test]
fn a_root_level_mount_dest_is_refused() {
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/data"), Mode::Rw));
    let host_env = BTreeMap::new();
    let err = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .unwrap_err();
    assert!(matches!(err, mysbx::krun::Error::RootLevelDest { dest } if dest == "/data"));
}

#[test]
fn a_nix_root_dest_is_refused() {
    // /nix's only surface is the store share's slot: the generic
    // placement would mount a tmpfs over /nix and hide the baked
    // /nix/store link, killing every store-backed payload. A dest
    // below /nix/store itself stays fine (the store share IS it).
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/nix/var"), Mode::Ro));
    std::env::set_var("MYSBX_RESOLV_SOURCE", "tests/assets/krun/resolv.conf");
    let err = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &BTreeMap::new(),
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .unwrap_err();
    assert!(matches!(err, mysbx::krun::Error::NixShareRoot { dest } if dest == "/nix/var"));
}

#[test]
fn a_baked_link_path_dest_is_refused() {
    // /nix/store: the store share's contract path — a configured
    // mount cannot replace it (a mount UNDER it is fine).
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/nix/store"), Mode::Rw));
    let host_env = BTreeMap::new();
    let err = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .unwrap_err();
    assert!(matches!(err, mysbx::krun::Error::ProtectedDest { dest: d } if d == "/nix/store"));
    // /mysbx-home is a single-component path: the root-level
    // refusal fires first (its link would sit on the ro root
    // anyway, and the init's case analysis never reaches a
    // protected-path check for it).
    cfg.mounts.clear();
    cfg.mounts
        .push(make_mount("/synth/data", Some("/mysbx-home"), Mode::Rw));
    let err = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .unwrap_err();
    assert!(matches!(err, mysbx::krun::Error::RootLevelDest { dest: d } if d == "/mysbx-home"));
}

#[test]
fn git_trust_adds_two_ro_shares_and_the_env_pin() {
    let cfg = base(true);
    let trust = mysbx::krun::GitTrust {
        global_host: "/synth/sidecar/gittrust/42/gitconfig",
        system_host: "/synth/sidecar/gittrust/42/system-gitconfig",
    };
    let mut p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    p.git_trust = Some(&trust);
    let argv = krun_argv(&cfg, &synth_repo(), &Payload::Shell, &BTreeMap::new(), &p).unwrap();
    let text = argv.join("\n");
    // Two ro shares at the podman contract's paths, ro enforced:
    assert!(text.contains("--ro-share\nstage-ro:gittrust-global@/etc/mysbx/gitconfig ro"));
    assert!(text.contains("--ro-share\nstage-ro:gittrust-system@/etc/gitconfig ro"));
    // GIT_CONFIG_GLOBAL last of the env, after PATH:
    assert!(text.contains("--env\nGIT_CONFIG_GLOBAL=/etc/mysbx/gitconfig"));
    let env_block = text
        .split("--env\n")
        .skip(1)
        .map(|s| s.lines().next().unwrap_or(""))
        .collect::<Vec<_>>();
    assert_eq!(
        env_block[env_block.len() - 1],
        "GIT_CONFIG_GLOBAL=/etc/mysbx/gitconfig"
    );
    // The staging binds must carry both host files into the ro tree:
    let binds = mysbx::krun::stage_binds(&cfg, &synth_repo(), &p);
    assert!(binds.contains(&(
        "/synth/sidecar/gittrust/42/gitconfig".to_owned(),
        true,
        "gittrust-global".to_owned()
    )));
    assert!(binds.contains(&(
        "/synth/sidecar/gittrust/42/system-gitconfig".to_owned(),
        true,
        "gittrust-system".to_owned()
    )));
}

#[test]
fn a_dest_below_an_unbaked_share_root_is_refused() {
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/var/lib/data"), Mode::Ro));
    let err = krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &BTreeMap::new(),
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .unwrap_err();
    assert!(
        matches!(err, mysbx::krun::Error::UnknownShareRoot { dest, root } if dest == "/var/lib/data" && root == "/var")
    );
    // A dest below a BAKED root passes the guard:
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/srv/data"), Mode::Ro));
    assert!(krun_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &BTreeMap::new(),
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    )
    .is_ok());
}
