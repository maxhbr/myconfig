// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Parity tests of the cloud-hypervisor builder (`backend =
//! "microvm-ch"`, docs/design/backends.md D11).
//!
//! The CH builder re-renders the krun spec exactly as the
//! microvm-qemu builder does (same sections, same order, same
//! `--kernel`/`--exit-channel` pair): for one input, both argvs are
//! byte-equal. The qemu goldens (tests/microvm_qemu.rs) pin the
//! spelling; these tests pin that the CH twin never drifts from it.

use mysbx::bwrap::{Payload, Workspace};
use mysbx::config::{Mode, Mount, Multiplexer};
use mysbx::krun::Params;
use mysbx::merge::Merged;
use mysbx::microvm_ch::microvm_ch_argv;
use mysbx::microvm_qemu::microvm_qemu_argv;
use mysbx::repo::Repo;
use std::collections::BTreeMap;
use std::path::PathBuf;

const KERNEL: &str = "/synth/kernel/bzImage";

fn synth_repo() -> Repo {
    Repo {
        root: PathBuf::from("/home/synth/repo"),
        sidecar: PathBuf::from("/home/synth/repo.mysbx"),
        git_dirs: Vec::new(),
        worktrees: None,
    }
}

fn base() -> Merged {
    Merged {
        backend: Some("microvm-ch".into()),
        network: mysbx::config::Network::Shared,
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
        multiplexer: Multiplexer::None,
        display: mysbx::config::Display::Off,
    }
}

fn params() -> Params<'static> {
    Params {
        rootfs: "/synth/rootfs",
        shell: "/synth/shell",
        tools_path: "/synth/bin",
        ca_bundle: None,
        mux_entry: None,
        workspace: Workspace::Live,
        memory: None,
        cpus: None,
        git_trust: None,
        scratch: None,
        store_disk: None,
        verbose: false,
        detachable: false,
        egress: None,
    }
}

fn assert_parity(cfg: &Merged, params: &Params<'_>) -> Vec<String> {
    // The shared spec builder reads the host resolver when the
    // network is shared: the qemu fixture pins it for both.
    std::env::set_var(
        "MYSBX_RESOLV_SOURCE",
        "tests/assets/microvm-qemu/resolv.conf",
    );
    let host_env = BTreeMap::new();
    let repo = synth_repo();
    let ch = microvm_ch_argv(cfg, &repo, &Payload::Shell, &host_env, params, KERNEL, &[])
        .unwrap_or_else(|e| panic!("the CH argv builds: {e}"));
    let qemu = microvm_qemu_argv(cfg, &repo, &Payload::Shell, &host_env, params, KERNEL, &[])
        .unwrap_or_else(|e| panic!("the qemu argv builds: {e}"));
    assert_eq!(ch, qemu, "the CH argv drifted from the qemu twin's");
    ch
}

/// Bless/write the golden of a CH argv (the krun/qemu suites'
/// convention): MYSBX_BLESS_GOLDEN=1 rewrites the file, otherwise
/// the argv must match it byte for byte, one argument per line.
fn assert_golden(rel: &str, argv: &[String]) {
    let path = format!("tests/assets/microvm-ch/{rel}");
    let mut rendered = String::new();
    for arg in argv {
        rendered.push_str(arg);
        rendered.push('\n');
    }
    if std::env::var_os("MYSBX_BLESS_GOLDEN").is_some() {
        std::fs::write(&path, &rendered).unwrap_or_else(|e| panic!("cannot bless {path}: {e}"));
        return;
    }
    let expected = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("cannot read the golden {path}: {e}"));
    assert_eq!(
        rendered, expected,
        "the CH argv drifted from the golden {path} (bless with \
         MYSBX_BLESS_GOLDEN=1 after an INTENDED change)"
    );
}

#[test]
fn proxy_only_is_accepted_by_the_builder_with_the_vsock_channel() {
    // The C2 lift (bd myconfig-fo1r): the CH twin renders the
    // proxy-only channel as `--network restricted` plus the
    // `--egress-socket`/`--egress-port` pair — the same launcher
    // grammar the krun twin defined, a different transport behind
    // it (the hybrid vsock: no NIC, one mapped port, the launcher's
    // relay child). The guest env carries the no-NIC vsock-bridge
    // marker (the krun spelling: it names the shared guest CLASS,
    // not a backend) and the guest-loopback `MYSBX_EGRESS_PROXY`.
    // No `--network none`, no `MYSBX_MICROVM_NET=1` — the NIC is
    // GONE, that is the fence.
    let mut cfg = base();
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
    let p = Params {
        egress: Some(&channel),
        ..params()
    };
    std::env::set_var(
        "MYSBX_RESOLV_SOURCE",
        "tests/assets/microvm-qemu/resolv.conf",
    );
    let host_env = BTreeMap::new();
    let argv = microvm_ch_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &p,
        KERNEL,
        &[],
    )
    .unwrap_or_else(|e| panic!("the restricted CH argv builds: {e}"));
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
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_KRUN_NET=restricted"));
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_EGRESS_PROXY=http://127.0.0.1:4000"));
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--env" && w[1] == "MYSBX_NETWORK=proxy-only"));
    assert!(!argv.iter().any(|a| a == "none"));
    assert!(
        !argv
            .windows(2)
            .any(|w| w[0] == "--env" && w[1] == "MYSBX_MICROVM_NET=1"),
        "the restricted guest gets no NIC marker — passt is never spawned: {argv:?}"
    );
    assert!(
        !argv.iter().any(|a| a.contains("aaaa-mysbx")),
        "the relay pin never rides the CH argv — the launcher's wrapper pin (MYSBX_MICROVM_CH_EGRESS_RELAY) serves it: {argv:?}"
    );
    assert_golden("egress-proxy-only.txt", &argv);
}

#[test]
fn a_channel_with_a_table_renders_like_a_plain_brokered_run_on_the_builder() {
    // The CONNECT table rides the SAME channel the model lane uses
    // (bd myconfig-ger7 — n8kv.2 C7a): the hybrid-vsock transport is
    // a transparent byte pipe to the per-run broker, which polices
    // the table on every backend — so a channel with rows renders
    // exactly like a plain brokered one at the argv level. The
    // public flag stays a bubblewrap-only lane (network_public:
    // false, the fvi gate refuses it upstream).
    for (domains, ports) in [
        (vec!["api.openai.com".to_string()], vec![443]),
        (vec![], vec![443]),
    ] {
        let mut cfg = base();
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
        let p = Params {
            egress: Some(&channel),
            ..params()
        };
        let host_env = BTreeMap::new();
        let argv = microvm_ch_argv(
            &cfg,
            &synth_repo(),
            &Payload::Shell,
            &host_env,
            &p,
            KERNEL,
            &[],
        )
        .expect("a table-carrying channel renders on microvm-ch");
        let plain = {
            let plain_channel = mysbx::bwrap::EgressChannel {
                allow_domains: &[],
                connect_ports: &[],
                ..channel
            };
            let plain_p = Params {
                egress: Some(&plain_channel),
                ..params()
            };
            microvm_ch_argv(
                &cfg,
                &synth_repo(),
                &Payload::Shell,
                &host_env,
                &plain_p,
                KERNEL,
                &[],
            )
            .expect("the plain brokered channel renders on microvm-ch")
        };
        let strip = |argv: &[String]| -> Vec<String> {
            argv.iter()
                .filter(|a| !a.starts_with("HTTP_PROXY") && !a.starts_with("http_proxy"))
                .cloned()
                .collect()
        };
        assert_eq!(strip(&argv), strip(&plain));
    }
}

#[test]
fn a_channel_less_brokered_run_is_still_refused_by_the_builder() {
    // The fail-closed complement of the lift: a brokered profile
    // whose Params carry NO channel (a run that lost its broker
    // pins between the pipeline's gate and the builder) still
    // fails closed in `spec_of` — never a shared passt net. The
    // refusal itself lives in the SHARED spec builder, so the
    // parity of the refusal is pinned by the krun/qemu suites;
    // this test pins that the CH renderer propagates it (it calls
    // `krun::spec_of` verbatim).
    let mut cfg = base();
    cfg.network = mysbx::config::Network::ProxyOnly;
    let p = params();
    let host_env = BTreeMap::new();
    let err = microvm_ch_argv(
        &cfg,
        &synth_repo(),
        &Payload::Shell,
        &host_env,
        &p,
        KERNEL,
        &[],
    )
    .expect_err("a channel-less brokered CH run must be refused");
    assert!(matches!(err, mysbx::krun::Error::ProxyOnlyRefused), "{err}");
}

#[test]
fn same_argv_as_qemu_minimal() {
    assert_parity(&base(), &params());
}

#[test]
fn same_argv_as_qemu_with_mounts_and_state_dirs() {
    let mut cfg = base();
    cfg.mounts.push(Mount {
        file: false,
        path: "/home/synth/data".into(),
        dest: None,
        mode: Mode::Ro,
    });
    cfg.mounts.push(Mount {
        file: false,
        path: "/synth/data".into(),
        dest: Some("/srv/data".into()),
        mode: Mode::Rw,
    });
    cfg.state_dirs.push(".cache".into());
    assert_parity(&cfg, &params());
}

#[test]
fn a_baked_store_run_declares_the_rw_device_its_shares_ride() {
    // The baked run has NO read-only share (the erofs disk replaces
    // the host store share) but rw shares on stage-rw: the device
    // must be declared, or every rw share names a missing tag.
    let mut p = params();
    p.store_disk = Some("/synth/store-disk.img");
    let argv = assert_parity(&base(), &p);
    assert!(
        !argv.iter().any(|a| a == "--ro-share"),
        "the fixture has no read-only share: {argv:?}"
    );
    assert!(argv.iter().any(|a| a == "--rw-share"));
    assert!(argv
        .windows(2)
        .any(|w| w[0] == "--rw-device" && w[1].starts_with("stage-rw=")));
}
