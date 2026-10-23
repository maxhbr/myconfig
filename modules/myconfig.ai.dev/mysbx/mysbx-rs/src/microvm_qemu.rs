// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The native-qemu backend spec builder (`backend =
//! "microvm-qemu"`, docs/design/backends.md D9, bd myconfig-tvry).
//!
//! The krun builder's twin: the SAME staged-spec argv — every
//! section, check and refusal of `krun.rs` carries over verbatim,
//! because the guest side is SHARED (the krun rootfs's baked
//! `bin/mysbx-init` mounts virtiofs devices by tag and reads the
//! same manifest), and only the VMM differs. What the qemu
//! launcher needs that libkrun bundles itself:
//!
//! - `--kernel PATH` — the pinned bzImage libkrunfw would have
//!   provided. A run names `MYSBX_MICROVM_QEMU_KERNEL` or the
//!   builder refuses (the launcher-pin rule, backends.md D9).
//! - `--exit-channel` — libkrun returns the payload's exit code
//!   from `krun_start_enter`; a stock qemu guest PANICS when
//!   PID 1 exits, so the launcher adds a virtio-serial port and
//!   the guest init reports the code over it before halting.
//!
//! Section order (fixed, like krun.rs's): `--cpus/--ram`,
//! `--verbose`, `--rootfs`, `--kernel`, `--init`, the staged
//! devices + manifest, the shares, the env, the overlay files,
//! `--network none` (shared is silent — SLiRP is the launcher's
//! default, exactly as TSI is krun's), the scratch, `--chdir`,
//! `--` payload. `krun::krun_argv` stays byte-identical; this
//! module re-renders its own spec and never touches the krun
//! goldens.

use crate::bwrap::Payload;
use crate::krun::{self, Params};
use crate::merge::Merged;
use crate::repo::Repo;

pub type Error = krun::Error;

/// The qemu backend's builder (D9): the merged config, the repo
/// and the run's pins become the mysbx-microvm-qemu launcher argv.
/// The kernel pin is the one input krun does not carry — libkrun
/// bundles its kernel, qemu needs one handed over.
pub fn microvm_qemu_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
    host_env: &krun::HostEnv,
    params: &Params<'_>,
    kernel: &str,
) -> Result<Vec<String>, Error> {
    // The krun spec, verbatim: every refusal (multiplexer, pids
    // limit, share roots, hidden mounts, the limit grammar) and
    // every section (env, shares, trust overlays, scratch) is the
    // shared contract — this builder only RE-RENDERS it.
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
    // The qemu-specific pair (D9): the kernel libkrun bundles and
    // the exit-code channel a libkrun caller gets from
    // krun_start_enter itself.
    argv.push("--kernel".to_owned());
    argv.push(kernel.to_owned());
    argv.push("--exit-channel".to_owned());
    argv.push("--init".to_owned());
    argv.push(spec.init.clone());
    let ro_slots = spec.shares.iter().filter(|s| s.read_only).count();
    let rw_slots = spec.shares.iter().filter(|s| !s.read_only).count();
    if ro_slots > 0 {
        argv.push("--ro-device".to_owned());
        argv.push(format!("{}={}/ro", krun::STAGE_RO_TAG, krun::STAGE_ROOT));
    }
    if rw_slots > 0 {
        argv.push("--rw-device".to_owned());
        argv.push(format!("{}={}/rw", krun::STAGE_RW_TAG, krun::STAGE_ROOT));
    }
    if ro_slots > 0 {
        argv.push("--manifest".to_owned());
        argv.push(format!("{}/ro/{}", krun::STAGE_ROOT, krun::MANIFEST_SLOT));
    }
    for share in &spec.shares {
        let device = krun::share_device(share.slot.as_str(), share.read_only);
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
    for (path, mode, data) in &spec.overlays {
        argv.push("--krun-overlay".to_owned());
        argv.push(format!("trustfs@{path}:{mode:o}:{data}"));
    }
    // `network = false` (D9): NO netdev at all — the same silent
    // shared default as krun's (SLiRP there, TSI here), the same
    // explicit refusal of the denied mode.
    if !spec.network {
        argv.push("--network".to_owned());
        argv.push("none".to_owned());
    }
    if let Some(scratch) = &spec.scratch {
        argv.push("--scratch".to_owned());
        argv.push(scratch.clone());
        argv.push("--env".to_owned());
        argv.push("MYSBX_KRUN_SCRATCH=1".to_owned());
    }
    argv.push("--chdir".to_owned());
    argv.push(spec.workdir.clone());
    argv.push("--".to_owned());
    argv.extend(spec.payload.iter().cloned());
    Ok(argv)
}

/// The predicate form of the placement rule — the SAME table the
/// shared guest init enforces, so callers that CHOOSE a path ask
/// the backend instead of re-deriving it (the krun twin's
/// `krun::placeable_share_root`).
pub fn placeable_share_root(sandbox_path: &str) -> Option<&'static str> {
    krun::placeable_share_root(sandbox_path)
}
