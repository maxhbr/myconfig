// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The cloud-hypervisor backend spec builder (`backend =
//! "microvm-ch"`, docs/design/backends.md, bd myconfig-9wkp.7).
//!
//! The microvm-qemu builder's twin: the SAME staged-spec argv —
//! every section, check and refusal of `krun.rs` carries over
//! verbatim, because the guest side is SHARED (the krun rootfs's
//! baked `bin/mysbx-init` mounts virtiofs devices by tag and reads
//! the same manifest), and only the VMM differs. What the CH
//! launcher needs beyond libkrun:
//!
//! - `--kernel PATH` — the pinned bzImage libkrunfw would have
//!   provided (the SAME kernel the qemu arm pins — one kernel
//!   serves both VM backends, bd myconfig-9wkp.3).
//! - `--exit-channel` — libkrun returns the payload's exit code
//!   from `krun_start_enter`; CH has no multiport virtio-serial,
//!   so the launcher wires the 16550 to a file and the guest init
//!   writes `mysbx-exit:N` to it before powering off (bd
//!   myconfig-9wkp.4).
//!
//! Section order (fixed, the qemu twin's): `--cpus/--ram`,
//! `--verbose`, `--rootfs`, `--kernel`, `--init`, the staged
//! devices + manifest, the shares, the env plus the backend pins
//! (MYSBX_MICROVM_CH_ENV), `--network none` (shared is silent —
//! passt mimics SLiRP, the launcher's default), the scratch,
//! `--chdir`, `--` payload. The opt-in `--nested-virt` switch
//! (config.md D27) sits with the CH-only pair; the launcher forces
//! `nested=off` on every plain run — CH defaults it ON, the exact
//! inverse of qemu.

use crate::bwrap::Payload;
use crate::krun::{self, Params};
use crate::merge::Merged;
use crate::repo::Repo;

pub type Error = krun::Error;

/// The CH backend's builder: the merged config, the repo and the
/// run's pins become the mysbx-microvm-ch launcher argv. The kernel
/// pin is the one input krun does not carry — libkrun bundles its
/// kernel, CH needs one handed over.
pub fn microvm_ch_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
    host_env: &krun::HostEnv,
    params: &Params<'_>,
    kernel: &str,
    backend_env: &[String],
) -> Result<Vec<String>, Error> {
    // The krun spec, verbatim: every refusal and every section is
    // the shared contract — this builder only RE-RENDERS it.
    let spec = krun::spec_of(cfg, repo, payload, host_env, params)?;

    let mut argv = Vec::with_capacity(64);
    argv.push("--cpus".to_owned());
    argv.push(spec.cpus.to_string());
    argv.push("--ram".to_owned());
    argv.push(spec.ram_mib.to_string());
    if spec.verbose {
        argv.push("--verbose".to_owned());
    }
    argv.push("--rootfs".to_owned());
    argv.push(spec.rootfs.clone());
    // The CH-specific pair: the kernel libkrun bundles and the
    // 16550 exit channel a libkrun caller gets from
    // krun_start_enter itself.
    argv.push("--kernel".to_owned());
    argv.push(kernel.to_owned());
    argv.push("--exit-channel".to_owned());
    // Nested virtualization (docs/design/config.md D27): the
    // launcher's flag switches the CPU line to nested=on — CH
    // defaults nested ON, so the launcher forces OFF on every
    // plain run (the exact inverse of the qemu twin's
    // vmx=off,svm=off masking). Off by default: nested VMX widens
    // the host KVM attack surface.
    if cfg.nested_virt {
        argv.push("--nested-virt".to_owned());
    }
    argv.push("--init".to_owned());
    argv.push(spec.init.clone());
    let ro_slots = spec.shares.iter().filter(|s| s.read_only).count();
    let rw_slots = spec.shares.iter().filter(|s| !s.read_only).count();
    // The manifest's device (the qemu twin's reasoning verbatim):
    // emitted whenever the manifest has records to carry.
    let manifest_needed = ro_slots > 0 || !spec.env.is_empty();
    if manifest_needed {
        argv.push("--ro-device".to_owned());
        argv.push(format!("{}={}/ro", krun::STAGE_RO_TAG, krun::STAGE_ROOT));
    }
    if rw_slots > 0 {
        argv.push("--rw-device".to_owned());
        argv.push(format!("{}={}/rw", krun::STAGE_RW_TAG, krun::STAGE_ROOT));
    }
    if manifest_needed {
        argv.push("--manifest".to_owned());
        argv.push(format!("{}/ro/{}", krun::STAGE_ROOT, krun::MANIFEST_SLOT));
    }
    for share in &spec.shares {
        // The trust shares ride the RO staging tree (bd
        // myconfig-govd) — CH has no in-memory trustfs device, the
        // launcher REFUSES `--krun-overlay` flags (the qemu twin's
        // rule).
        let device =
            if krun::share_device(share.slot.as_str(), share.read_only) == krun::TRUSTFS_TAG {
                krun::STAGE_RO_TAG
            } else {
                krun::share_device(share.slot.as_str(), share.read_only)
            };
        argv.push(if share.read_only {
            "--ro-share".to_owned()
        } else {
            "--rw-share".to_owned()
        });
        argv.push(format!(
            "{device}:{}@{} {}",
            share.slot,
            share.sandbox_path,
            if share.read_only { "ro" } else { "rw" }
        ));
    }
    for (k, v) in &spec.env {
        argv.push("--env".to_owned());
        argv.push(format!("{k}={v}"));
    }
    // Backend pins (MYSBX_MICROVM_CH_ENV, the qemu twin's
    // MYSBX_MICROVM_QEMU_ENV rule): after the spec's env, so a pin
    // wins over a configured value. The model endpoint is the
    // first consumer: the passt guest's loopback is its own, so
    // host-loopback URLs are repointed at the 10.0.2.2 gateway,
    // which NATs to the host loopback — the SAME SLiRP plan the
    // qemu arm carries (bd myconfig-9wkp.5).
    for entry in backend_env {
        argv.push("--env".to_owned());
        argv.push(entry.clone());
    }
    // The SLiRP NIC marker (bd myconfig-k106, the qemu twin's): the
    // guest init configures the NIC only when this env rides the
    // manifest — the passt NIC uses the same static plan.
    if spec.network {
        argv.push("--env".to_owned());
        argv.push("MYSBX_MICROVM_NET=1".to_owned());
    }
    // The nested-virt marker (docs/design/config.md D27): the guest
    // init REFUSES the run early when the VM was booted without
    // VMX/SVM after all — /dev/kvm must exist in a nested-virt run.
    if cfg.nested_virt {
        argv.push("--env".to_owned());
        argv.push("MYSBX_MICROVM_NESTED_VIRT=1".to_owned());
    }
    // NO `--krun-overlay` flags (the launcher refuses them); the
    // trust rides the staged slots above.
    // `network = false` (the qemu twin's arm): NO net device at all
    // — the launcher attaches none, the bwrap wrap unshare-nets
    // the chain.
    if !spec.network {
        argv.push("--network".to_owned());
        argv.push("none".to_owned());
    }
    if let Some(scratch) = &spec.scratch {
        argv.push("--scratch".to_owned());
        argv.push(scratch.clone());
        argv.push("--env".to_owned());
        argv.push("MYSBX_VM_SCRATCH=1".to_owned());
    }
    // The guest-own store disk (docs/design/guest-store.md): the
    // SAME emission the krun and qemu renderers make — the CH
    // launcher attaches the erofs image as a read-only virtio-blk
    // device and the init mounts it at /nix.
    if let Some(disk) = &spec.store_disk {
        argv.push("--store-disk".to_owned());
        argv.push(disk.clone());
        argv.push("--env".to_owned());
        argv.push(crate::STORE_DISK_ENV_RECORD.to_owned());
    }
    argv.push("--chdir".to_owned());
    argv.push(spec.workdir.clone());
    argv.push("--".to_owned());
    argv.extend(spec.payload.iter().cloned());
    Ok(argv)
}

/// The predicate form of the placement rule — the SAME table the
/// shared guest init enforces (the qemu twin's).
pub fn placeable_share_root(sandbox_path: &str) -> Option<&'static str> {
    krun::placeable_share_root(sandbox_path)
}
