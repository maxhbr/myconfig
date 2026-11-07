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
        network: true,
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
