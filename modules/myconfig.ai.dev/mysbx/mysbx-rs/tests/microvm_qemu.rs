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
//! The ONE deliberate share-record divergence: a git-trust run
// maps the two trust shares onto `stage-ro` (libkrun's in-memory
// `trustfs` device does not exist on qemu, bd myconfig-govd) —
// pinned by the git-trust golden, never hit by the parity tests
// (they run without a trust).

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
    let network = if network {
        mysbx::config::Network::Shared
    } else {
        mysbx::config::Network::None
    };
    Merged {
        backend: Some("microvm-qemu".into()),
        network,
        nested_virt: false,
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
        scratch_size: None,
        cpus: None,
        pids_limit: None,
        store: Some(mysbx::config::Store::Host),
        profile: None,
        default_agent: None,
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
        store_disk: None,
        verbose: false,
        detachable: false,
        egress: None,
        npmrc: None,
        node_major: None,
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
    microvm_qemu_argv(cfg, repo, payload, &host_env, params, KERNEL, &[])
        .unwrap_or_else(|e| panic!("the argv builds: {e}"))
}

#[test]
fn proxy_only_without_a_channel_is_refused_by_the_builder() {
    // D20 (bd myconfig-krab): a `network = "proxy-only"` run whose
    // params carry NO channel is refused — the fail-closed half of
    // the lift. A channel-less brokered run would have no forward
    // to render, and the refusal fires in `spec_of` before any
    // argv exists, never a silent shared network.
    let mut cfg = base(true);
    cfg.network = mysbx::config::Network::ProxyOnly;
    let p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    let host_env = BTreeMap::new();
    let err = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &p,
        KERNEL,
        &[],
    )
    .expect_err("a channel-less proxy-only qemu run must be refused");
    assert!(matches!(err, mysbx::krun::Error::ProxyOnlyRefused), "{err}");
}

#[test]
fn a_channel_without_the_profile_is_refused_by_the_builder() {
    // The inverse gate (bd myconfig-krab): the channel renders only
    // under its profile — a shared run carrying one is a wiring
    // error the builder refuses, not a restricted net silently
    // attached to a shared config.
    let mut cfg = base(true);
    let channel = mysbx::bwrap::EgressChannel {
        socket_dir: "/run/user/1000/mysbx-egress/1234",
        socket: "litellm.sock",
        port: 4000,
        relay_bin: "/synth/store/aaaa-mysbx/bin/mysbx-egress-relay",
        etc_dir: "/synth/store/bbbb-mysbx-egress-etc",
        allow_domains: &[],
        connect_ports: &[],
        public: false,
    };
    let p = mysbx::krun::Params {
        egress: Some(&channel),
        ..params("/synth/rootfs", "/synth/shell", None, Workspace::Live)
    };
    let host_env = BTreeMap::new();
    let err = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &p,
        KERNEL,
        &[],
    )
    .expect_err("a channel on a shared qemu run must be refused");
    assert!(
        matches!(err, mysbx::krun::Error::EgressChannelWithoutProfile),
        "{err}"
    );
}

#[test]
fn a_channel_with_a_table_renders_like_a_plain_brokered_run_on_the_builder() {
    // The CONNECT table rides the SAME channel the model lane uses
    // (bd myconfig-ger7 — n8kv.2 C7a): the guestfwd transport is a
    // transparent byte pipe to the per-run broker, which polices
    // the table on every backend — so a channel with rows renders
    // exactly like a plain brokered one at the argv level. The
    // public flag stays a bubblewrap-only lane (network_public:
    // false, the fvi gate refuses it upstream).
    for (domains, ports) in [
        (vec!["api.openai.com".to_string()], vec![443]),
        (vec![], vec![443]),
    ] {
        let mut cfg = base(true);
        cfg.network = mysbx::config::Network::ProxyOnly;
        let channel = mysbx::bwrap::EgressChannel {
            socket_dir: "/run/user/1000/mysbx-egress/1234",
            socket: "litellm.sock",
            port: 4000,
            relay_bin: "/synth/store/aaaa-mysbx/bin/mysbx-egress-relay",
            etc_dir: "/synth/store/bbbb-mysbx-egress-etc",
            allow_domains: &domains,
            connect_ports: &ports,
            public: false,
        };
        let p = mysbx::krun::Params {
            egress: Some(&channel),
            ..params("/synth/rootfs", "/synth/shell", None, Workspace::Live)
        };
        let host_env = BTreeMap::new();
        let argv = microvm_qemu_argv(
            &cfg,
            &synth_repo(),
            &Payload::Shell,
            &host_env,
            &p,
            KERNEL,
            &[],
        )
        .expect("a table-carrying channel renders on qemu");
        let plain = {
            let plain_channel = mysbx::bwrap::EgressChannel {
                allow_domains: &[],
                connect_ports: &[],
                ..channel
            };
            let plain_p = mysbx::krun::Params {
                egress: Some(&plain_channel),
                ..params("/synth/rootfs", "/synth/shell", None, Workspace::Live)
            };
            microvm_qemu_argv(
                &cfg,
                &synth_repo(),
                &Payload::Shell,
                &host_env,
                &plain_p,
                KERNEL,
                &[],
            )
            .expect("the plain brokered channel renders on qemu")
        };
        // The launcher argv is byte-identical: the table rides the
        // broker's own argv (spawned host-side), not the VM's. The
        // guest env differs ONLY by the E8 injection below — all
        // six names of it, each an `--env NAME=VALUE` PAIR.
        let strip = |argv: &[String]| -> Vec<String> {
            let injected = |value: &str| {
                value.starts_with("HTTP_PROXY")
                    || value.starts_with("http_proxy")
                    || value.starts_with("HTTPS_PROXY")
                    || value.starts_with("https_proxy")
                    || value.starts_with("NO_PROXY")
                    || value.starts_with("no_proxy")
                    || value.starts_with("NODE_USE_ENV_PROXY=")
            };
            let mut out: Vec<String> = Vec::new();
            let mut i = 0;
            while i < argv.len() {
                if argv[i] == "--env" && i + 1 < argv.len() && injected(&argv[i + 1]) {
                    i += 2;
                } else {
                    out.push(argv[i].clone());
                    i += 1;
                }
            }
            out
        };
        assert_eq!(strip(&argv), strip(&plain));
    }
}

#[test]
fn proxy_only_golden_renders_the_restricted_channel() {
    // The lift's argv shape (bd myconfig-krab — n8kv.2 C4): the
    // launcher gets `--network restricted` plus the channel pair —
    // the broker socket qemu's guestfwd relay connects, and the
    // port the guest dials on the SLiRP gateway. The guest env
    // carries the restricted NIC marker (no default route, no DNS
    // proxy — the init's restricted arm) and the guest-side
    // `MYSBX_EGRESS_PROXY` spelling (the 10.0.2.2 gateway, never
    // `127.0.0.1`: the guest's loopback is its own). No
    // `--network none` — the restricted netdev IS the fence.
    let mut cfg = base(false);
    cfg.network = mysbx::config::Network::ProxyOnly;
    let channel = mysbx::bwrap::EgressChannel {
        socket_dir: "/run/user/1000/mysbx-egress/1234",
        socket: "litellm.sock",
        port: 4000,
        relay_bin: "/synth/store/aaaa-mysbx/bin/mysbx-egress-relay",
        etc_dir: "/synth/store/bbbb-mysbx-egress-etc",
        allow_domains: &[],
        connect_ports: &[],
        public: false,
    };
    let p = mysbx::krun::Params {
        egress: Some(&channel),
        ..params("/synth/rootfs", "/synth/shell", None, Workspace::Live)
    };
    let host_env = BTreeMap::new();
    let argv = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &p,
        KERNEL,
        &[],
    )
    .unwrap_or_else(|e| panic!("the restricted argv builds: {e}"));
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--network" && w[1] == "restricted"));
    assert!(
        argv.windows(2).any(|w| w[0] == "--egress-socket"
            && w[1] == "/run/user/1000/mysbx-egress/1234/litellm.sock"),
        "the socket is the broker dir plus the pinned name: {argv:?}"
    );
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--egress-port" && w[1] == "4000"));
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_MICROVM_NET=restricted"));
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_EGRESS_PROXY=http://10.0.2.2:4000"));
    assert!(!argv.iter().any(|a| a == "none"));
    assert_golden("egress-proxy-only.txt", &argv);
}

#[test]
fn proxy_only_golden_matches_the_krun_env_parity() {
    // The guest env parity: the restricted run's `MYSBX_NETWORK` is
    // `proxy-only` (the merged profile, not a `none` lie) and the
    // relay binary rides NO argv piece — the launcher reads the
    // wrapper's pin (`MYSBX_MICROVM_QEMU_EGRESS_RELAY`), so the
    // builder's argv names only paths inside the views it builds.
    let mut cfg = base(false);
    cfg.network = mysbx::config::Network::ProxyOnly;
    let channel = mysbx::bwrap::EgressChannel {
        socket_dir: "/run/user/1000/mysbx-egress/1234",
        socket: "litellm.sock",
        port: 4000,
        relay_bin: "/synth/store/aaaa-mysbx/bin/mysbx-egress-relay",
        etc_dir: "/synth/store/bbbb-mysbx-egress-etc",
        allow_domains: &[],
        connect_ports: &[],
        public: false,
    };
    let p = mysbx::krun::Params {
        egress: Some(&channel),
        ..params("/synth/rootfs", "/synth/shell", None, Workspace::Live)
    };
    let host_env = BTreeMap::new();
    let argv = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &p,
        KERNEL,
        &[],
    )
    .unwrap_or_else(|e| panic!("the argv builds: {e}"));
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_NETWORK=proxy-only"));
    assert!(
        !argv.iter().any(|a| a.contains("aaaa-mysbx")),
        "the relay pin never rides the builder argv: {argv:?}"
    );
}

#[test]
fn nested_virt_rides_the_qemu_only_flag_and_marker() {
    // docs/design/config.md D27: the opt-in carries exactly TWO argv
    // pieces — the launcher's `--nested-virt` switch (it boots -cpu
    // host, accel=kvm) and the manifest marker the guest init refuses
    // a /dev/kvm-less boot on. The plain argv stays byte-identical to
    // the default argv (no flag, no marker).
    let mut cfg = base(true);
    cfg.nested_virt = true;
    let argv = run(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert!(argv.iter().any(|a| a == "--nested-virt"), "{argv:?}");
    assert!(
        argv.windows(2)
            .any(|w| w[0] == "--env" && w[1] == "MYSBX_MICROVM_NESTED_VIRT=1"),
        "{argv:?}"
    );
    assert_golden("nested-virt.txt", &argv);

    // Without the key: NEITHER piece — the default argv is the
    // minimal golden, byte for byte.
    let plain = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert!(!plain.iter().any(|a| a == "--nested-virt"), "{plain:?}");
    assert!(
        !plain
            .windows(2)
            .any(|w| w[1] == "MYSBX_MICROVM_NESTED_VIRT=1"),
        "{plain:?}"
    );
    assert_eq!(
        plain,
        run(
            &base(true),
            &synth_repo(),
            &Payload::Shell,
            &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
        ),
        "an unset nested-virt builds the same argv as a key-less config"
    );
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
fn golden_git_trust_rides_the_stage_tree_not_overlay_flags() {
    // bd myconfig-govd: libkrun serves the trust from its in-memory
    // trustfs device (the krun backend's --krun-overlay flags); the
    // qemu launcher REFUSES overlay flags, so the trust shares map
    // onto the RO STAGING TREE (lib.rs writes the texts into
    // <stage>/ro/<slot> before the run) and the argv carries NO
    // --krun-overlay at all.
    let trust = mysbx::krun::GitTrust {
        global_text: "safe.directory = /tmp/mysbx-shares/stage-rw/workspace\n".to_owned(),
        system_text: "safe.directory = /tmp/mysbx-shares/stage-rw/workspace\n".to_owned(),
    };
    let mut p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    p.git_trust = Some(&trust);
    let argv = run(&base(true), &synth_repo(), &Payload::Shell, &p);
    let text = argv.join("\n");
    // NO overlay flag — the launcher refuses them; a builder that
    // emits one makes every real run exit 125 before the boot.
    assert!(
        !text.contains("--krun-overlay"),
        "the qemu argv must not carry overlay flags: {text}"
    );
    // The two trust shares exist — pointing at the RO STAGE device,
    // never at libkrun's trustfs tag.
    assert!(
        text.contains("--ro-share\nstage-ro:gittrust-global@/etc/mysbx/gitconfig ro"),
        "the global trust share must ride stage-ro: {text}"
    );
    assert!(
        text.contains("--ro-share\nstage-ro:gittrust-system@/etc/gitconfig ro"),
        "the system trust share must ride stage-ro: {text}"
    );
    assert!(
        !text.contains("trustfs"),
        "no trustfs device exists on qemu: {text}"
    );
    assert_golden("git-trust.txt", &argv);
}

#[test]
fn a_store_disk_param_replaces_the_host_store_share() {
    // docs/design/guest-store.md, D9's shared core: the baked run
    // carries NO store share (the erofs disk replaces it) and the
    // SAME --store-disk flag and announcement the krun renderer
    // emits — the guest side is one contract, two launchers.
    let mut p = params("/synth/rootfs", "/synth/shell", None, Workspace::Live);
    p.store_disk = Some("/synth/store-disk.img");
    let argv = run(&base(true), &synth_repo(), &Payload::Shell, &p);
    assert!(
        !argv
            .windows(2)
            .any(|w| w[0] == "--ro-share" && w[1] == "stage-ro:store@/nix/store ro"),
        "no host store share in a baked run"
    );
    let i = argv
        .iter()
        .position(|a| a == "--store-disk")
        .expect("the --store-disk flag");
    assert_eq!(argv[i + 1], "/synth/store-disk.img");
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_VM_STORE_DISK=1"));
    assert_golden("baked-store.txt", &argv);
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
    // myconfig-3nn), and MYSBX_MICROVM_NET the qemu-only SLiRP NIC
    // marker (bd myconfig-k106 — the krun backend's network is TSI,
    // no guest-side NIC config step exists there) — the TWO env
    // entries the renderers must differ in. Everything else is the
    // shared spec, byte for byte.
    let identity = |v: Vec<String>| {
        let mut out: Vec<String> = Vec::with_capacity(v.len());
        for a in v {
            // The MYSBX_BACKEND identity names the run's own backend
            // (bd myconfig-3nn); MYSBX_MICROVM_NET is the qemu-only
            // SLiRP NIC marker (bd myconfig-k106, riding as an --env
            // PAIR — drop the flag with its value, a bare filter
            // would leave a dangling --env behind).
            if a.starts_with("MYSBX_BACKEND=") || a.starts_with("MYSBX_MICROVM_NET=") {
                if a.starts_with("MYSBX_MICROVM_NET=") {
                    if out.last().map(|l| l == "--env").unwrap_or(false) {
                        out.pop();
                    }
                }
                continue;
            }
            out.push(a);
        }
        out
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
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_VM_SCRATCH=1"));
    // Without the param: no flag, no announcement.
    let argv = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert!(!argv.iter().any(|a| a == "--scratch"));
    assert!(!argv.windows(2).any(|w| w[1] == "MYSBX_VM_SCRATCH=1"));
}

#[test]
fn backend_env_pins_ride_after_the_layers_and_win_over_config() {
    // MYSBX_MICROVM_QEMU_ENV (bd myconfig-mzys): the pins are emitted
    // as --env after the config layers' `[env]` (so a pin wins over a
    // configured value) and before the SLiRP NIC marker and the
    // infrastructure entries the shared spec owns. The model
    // endpoint is the consumer: the SLiRP guest's loopback is its
    // own, the pins repoint the URL at the gateway `10.0.2.2`.
    let mut cfg = base(true);
    cfg.env.insert(
        "OPENAI_BASE_URL".to_owned(),
        "http://localhost:4000/v1".to_owned(),
    );
    let pins = vec![
        "OPENAI_BASE_URL=http://10.0.2.2:4000/v1".to_owned(),
        "MYCONFIG_LITELLM_BASE_URL=http://10.0.2.2:4000/v1".to_owned(),
    ];
    let argv = microvm_qemu_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &BTreeMap::new(),
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
        KERNEL,
        &pins,
    )
    .unwrap();
    // Both the configured value and the pin appear — the pin AFTER
    // the layers, so the guest init's later export of the same key
    // wins (the manifest env is applied in record order).
    let positions: Vec<usize> = argv
        .iter()
        .enumerate()
        .filter(|(_, a)| a.starts_with("OPENAI_BASE_URL="))
        .map(|(i, _)| i)
        .collect();
    assert_eq!(
        positions.len(),
        2,
        "the configured value and the pin must both ride: {argv:?}"
    );
    assert_eq!(
        argv[positions[0]], "OPENAI_BASE_URL=http://localhost:4000/v1",
        "the config layer comes first"
    );
    assert_eq!(
        argv[positions[1]], "OPENAI_BASE_URL=http://10.0.2.2:4000/v1",
        "the pin comes after the layer and wins in the guest"
    );
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--env" && w[1] == "MYCONFIG_LITELLM_BASE_URL=http://10.0.2.2:4000/v1"));
    // The pins sit BEFORE the qemu-only SLiRP NIC marker — the same
    // placement rule the podman arm gives its MYSBX_PODMAN_ENV pins
    // (after the layers, before the infrastructure).
    let pin_pos = argv
        .iter()
        .position(|a| a == "MYCONFIG_LITELLM_BASE_URL=http://10.0.2.2:4000/v1")
        .unwrap();
    let net_pos = argv
        .iter()
        .position(|a| a == "MYSBX_MICROVM_NET=1")
        .expect("the shared run carries the SLiRP NIC marker");
    assert!(pin_pos < net_pos, "the pins must precede the NIC marker");
    // No pins, no divergence: the empty-pins argv is the golden
    // shape (the parity test pins it byte-for-byte).
    let bare = run(
        &base(true),
        &synth_repo(),
        &Payload::Shell,
        &params("/synth/rootfs", "/synth/shell", None, Workspace::Live),
    );
    assert!(!bare
        .windows(2)
        .any(|w| w[0] == "--env" && w[1].starts_with("OPENAI_BASE_URL=http://10.0.2.2")));
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
        &[],
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
        &[],
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
        &[],
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
        &[],
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
