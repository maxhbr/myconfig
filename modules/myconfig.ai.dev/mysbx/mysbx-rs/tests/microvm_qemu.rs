// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Golden argv tests of the native-qemu backend (`backend =
// "microvm-qemu"`, docs/design/backends.md D9, bd myconfig-tvry).
//!
//! The builder re-renders the krun spec (tests/krun.rs's twin): the
//! fixtures mirror `tests/assets/krun/` case for case, with the two
//! qemu-only additions D9 names — `--kernel` right after
//! `--rootfs`, and the `--exit-channel` flag of the virtio-serial
// port a stock qemu guest needs because PID-1 exit panics it.
//! Everything else — share records, the manifest pointer, the env
//! block, `--network none`, the scratch — must stay byte-equal to
// the krun spelling, which the `same_spec_as_krun_*` tests pin.

use mysbx::bwrap::{Payload, Workspace};
use mysbx::config::{Mode, Mount, Multiplexer};
use mysbx::krun::Params;
use mysbx::merge::Merged;
use mysbx::microvm_qemu::microvm_qemu_argv;
use mysbx::repo::Repo;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::path::PathBuf;

const KERNEL: &str = "/synth/kernel/bzImage";

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
        backend: Some("microvm-qemu".into()),
        network,
        mounts: Vec::new(),
        env: BTreeMap::new(),
        nix: BTreeMap::new(),
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
        verbose: false,
        detachable: false,
    }
}

fn assert_golden(rel: &str, argv: &[String]) {
    let path = format!("tests/assets/microvm-qemu/{rel}");
    let mut rendered = String::new();
    for arg in argv {
        rendered.push_str(arg);
        rendered.push('\n');
    }
    // The same bless convention as tests/krun.rs.
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
    // The resolver fixture pins the goldens (the shared spec builder
    // reads the host resolver when the network is shared).
    std::env::set_var(
        "MYSBX_RESOLV_SOURCE",
        "tests/assets/microvm-qemu/resolv.conf",
    );
    let host_env = BTreeMap::new();
    microvm_qemu_argv(cfg, repo, payload, &host_env, params, KERNEL)
        .unwrap_or_else(|e| panic!("the argv builds: {e}"))
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
                clone: std::path::Path::new("/synth/repo.mysbx/clones/s1"),
                worktrees: std::path::Path::new("/synth/repo.mysbx/clones/s1__worktrees"),
            },
        ),
    );
    assert_golden("clone-session.txt", &argv);
}

#[test]
fn golden_verbose_forwards_the_flag_to_the_launcher() {
    let argv = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &Params {
            verbose: true,
            ..params("/synth/rootfs", "/synth/shell", None, Workspace::Live)
        },
    );
    assert_golden("verbose.txt", &argv);
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

/// The krun goldens minus their first two sections (cpus/ram are
/// equal; rootfs/init/devices/manifest/shares/env/network/scratch/
/// chdir/payload are the shared spec) — the parity harness: strip
/// the qemu-only `--kernel`/`--exit-channel` pair and the two
/// argvs must agree byte for byte.
fn krun_parity_argv(cfg: &Merged, payload: &Payload, params: &Params<'_>) -> Vec<String> {
    std::env::set_var(
        "MYSBX_RESOLV_SOURCE",
        "tests/assets/microvm-qemu/resolv.conf",
    );
    let host_env = BTreeMap::new();
    let mut cfg = cfg.clone();
    cfg.backend = Some("krun".into());
    let argv = mysbx::krun::krun_argv(&cfg, &synth_repo(), payload, &host_env, params)
        .unwrap_or_else(|e| panic!("the krun argv builds: {e}"));
    argv.into_iter().filter(|a| a != "--verbose").collect()
}

#[test]
fn same_spec_as_krun_minimal() {
    let qemu = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    // Strip the qemu-only pieces: the --kernel VALUE pair and the
    // valueless --exit-channel switch. Everything else is the shared
    // spec.
    let stripped: Vec<String> = {
        let mut v = Vec::new();
        let mut skip_next = false;
        for a in qemu {
            if skip_next {
                skip_next = false;
                continue;
            }
            if a == "--kernel" {
                skip_next = true;
                continue;
            }
            if a == "--exit-channel" {
                continue;
            }
            v.push(a);
        }
        v
    };
    let krun = krun_parity_argv(
        &base(true),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    // The MYSBX_BACKEND identity names the run's own backend (bd
    // myconfig-3nn) — the ONE entry the two renderers must differ
    // in. Everything else is the shared spec, byte for byte.
    let identity = |v: Vec<String>| {
        v.into_iter()
            .filter(|a| !a.starts_with("MYSBX_BACKEND="))
            .collect::<Vec<_>>()
    };
    assert_eq!(identity(stripped), identity(krun));
}

#[test]
fn kernel_rides_right_after_the_rootfs() {
    // D9's fixed section order: --rootfs, --kernel, --exit-channel,
    // --init — the kernel is the rootfs's own pair (the VM's two
    // image inputs sit together).
    let argv = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    let i = argv
        .iter()
        .position(|a| a == "--rootfs")
        .expect("the --rootfs flag");
    assert_eq!(argv[i + 1], "/synth/rootfs");
    assert_eq!(argv[i + 2], "--kernel");
    assert_eq!(argv[i + 3], KERNEL);
    assert_eq!(argv[i + 4], "--exit-channel");
    assert_eq!(argv[i + 5], "--init");
    assert_eq!(argv[i + 6], "/bin/mysbx-init");
}

#[test]
fn network_none_names_the_flag_shared_is_silent() {
    // The same spelling as krun (the shared spec): `none` is the
    // only mode the argv names — shared is the launcher's SLiRP
    // default, exactly as TSI is krun's.
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
fn a_scratch_param_renders_the_flag_and_the_announcement() {
    // The shared spec's scratch (D7): the flag names the
    // launcher-view image, the announcement rides the manifest env.
    let mut p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    p.scratch = Some(mysbx::krun::SCRATCH_IMG);
    let argv = run(&base(true), &synth_repo(), &Payload::Shell, &p);
    let i = argv
        .iter()
        .position(|a| a == "--scratch")
        .expect("the --scratch flag");
    assert_eq!(argv[i + 1], mysbx::krun::SCRATCH_IMG);
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_KRUN_SCRATCH=1"));
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
fn a_pids_limit_is_refused() {
    // The shared refusal surface: a pids-limit the whole VM cannot
    // express is refused, never dropped silently (D9).
    let mut cfg = base(true);
    cfg.pids_limit = Some(100);
    let err = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &BTreeMap::new(),
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
        KERNEL,
    )
    .unwrap_err();
    assert_eq!(err, mysbx::krun::Error::PidsLimit { pids: 100 });
}

#[test]
fn a_session_multiplexer_without_an_entry_is_refused() {
    let mut cfg = base(true);
    cfg.multiplexer = Multiplexer::Tmux;
    let err = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &BTreeMap::new(),
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
        KERNEL,
    )
    .unwrap_err();
    assert_eq!(
        err,
        mysbx::krun::Error::MultiplexerUnavailable {
            multiplexer: Multiplexer::Tmux,
        }
    );
}

#[test]
fn a_dest_below_an_unbaked_share_root_is_refused() {
    // The shared placement rule (the guest init is shared): a dest
    // below a component the rootfs bakes no mountpoint for is a
    // builder refusal, never a run-time ENOENT.
    let mut cfg = base(true);
    cfg.mounts
        .push(make_mount("/synth/data", Some("/var/lib/data"), Mode::Ro));
    let err = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &BTreeMap::new(),
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
        KERNEL,
    )
    .unwrap_err();
    assert!(
        matches!(err, mysbx::krun::Error::UnknownShareRoot { ref dest, ref root }
            if dest == "/var/lib/data" && root == "/var"),
        "{err}"
    );
}

#[test]
fn limits_size_the_vm() {
    // The shared limit grammar (config.md D23): the keys override the
    // defaults through the same parser krun uses.
    let cfg = base(true);
    let mut p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    p.cpus = Some(Cow::from("4".to_owned()));
    p.memory = Some(Cow::from("8g".to_owned()));
    let argv = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &BTreeMap::new(),
        &p,
        KERNEL,
    )
    .unwrap();
    assert!(argv.windows(2).any(|w| w[0] == "--cpus" && w[1] == "4"));
    assert!(argv.windows(2).any(|w| w[0] == "--ram" && w[1] == "8192"));
}

#[test]
fn the_placeable_roots_are_the_shared_table() {
    // The predicate delegates to the shared table (the guest init
    // is the same rootfs): everything placeable for krun is
    // placeable here, everything refused there is refused here.
    for path in ["/tmp/repo", "/mysbx-home/.cache", "/srv/data", "/home/x"] {
        assert_eq!(
            mysbx::microvm_qemu::placeable_share_root(path),
            mysbx::krun::placeable_share_root(path),
            "{path}"
        );
    }
    for path in ["/nix/store/x", "/build/repo", "/var/lib/data", "/"] {
        assert_eq!(
            mysbx::microvm_qemu::placeable_share_root(path),
            None,
            "{path}"
        );
    }
}
