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
//! devices + manifest, the shares (the trust shares mapped onto
//! stage-ro, bd myconfig-govd — NO overlay flags on this
//! backend), the env plus the backend pins
//! (MYSBX_MICROVM_QEMU_ENV, bd myconfig-mzys), `--network none`
//! (shared is silent — SLiRP is the launcher's default, exactly as
//! TSI is krun's), the scratch, `--chdir`, `--` payload.
//! `krun::krun_argv` stays byte-identical; this module re-renders its
//! own spec and never touches the krun goldens.

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
    backend_env: &[String],
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
    // The manifest's device: the ro stage tree carries the SHARE
    // slots, and a baked-store run can leave it without a single ro
    // one (the store share was its only mandatory slot, the disk
    // replaces it) — but the MANIFEST itself still needs the device:
    // every env record rides it, and the no-manifest fallback is the
    // kernel cmdline, a route that exists only for cmdline-sized
    // debug runs (the 2048-byte finding). So the device + pointer
    // are emitted whenever the manifest has records to carry — env
    // is never empty in a real run — even with zero ro slots.
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
        // The trust shares (bd myconfig-govd): libkrun serves them
        // from the in-memory `trustfs` virtual device the launcher
        // registers the overlay files on — qemu has no such device,
        // and the launcher REFUSES `--krun-overlay` flags. The
        // trust therefore rides the RO STAGING TREE instead:
        // lib.rs writes the two texts as plain files into the
        // stage's slots before the run, and the shares point at
        // the stage-ro device like every other ro share — the
        // guest init's slot mount logic is unchanged.
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
    // Backend pins (MYSBX_MICROVM_QEMU_ENV, bd myconfig-mzys): after
    // the spec's env, so a pin wins over a configured value, and
    // before the SLiRP NIC marker — the same placement the podman
    // arm gives its MYSBX_PODMAN_ENV pins (src/podman_gvisor.rs
    // section 7). The model endpoint is the first consumer: the
    // SLiRP guest's loopback is its own, so the host-loopback URLs
    // the shared config carries are dead here and the pins repoint
    // them at the SLiRP gateway `10.0.2.2`, which maps to the host
    // loopback.
    for entry in backend_env {
        argv.push("--env".to_owned());
        argv.push(entry.clone());
    }
    // The SLiRP NIC marker (bd myconfig-k106): the guest init
    // configures the user-netdev NIC only when this env rides the
    // manifest — the krun backend sets none (its network is TSI,
    // no ethernet device exists in that guest).
    if spec.network {
        argv.push("--env".to_owned());
        argv.push("MYSBX_MICROVM_NET=1".to_owned());
    }
    // NO `--krun-overlay` flags (bd myconfig-govd): the launcher
    // refuses them (the in-memory trustfs device is libkrun's own),
    // and this renderer never emits what the launcher would reject
    // — the spec's overlays are served by the staged slots above
    // instead. The krun renderer keeps its own overlay emission
    // untouched (its launcher registers them on the root device).
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
    // The guest-own store disk (docs/design/guest-store.md): the SAME
    // emission the krun renderer makes (D9: the spec is shared, the
    // guest side is shared) — the qemu launcher attaches the erofs
    // image as a second read-only virtio-blk device and the init
    // mounts it at /nix; the run's shares carry no host store slot.
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
/// shared guest init enforces, so callers that CHOOSE a path ask
/// the backend instead of re-deriving it (the krun twin's
/// `krun::placeable_share_root`).
pub fn placeable_share_root(sandbox_path: &str) -> Option<&'static str> {
    krun::placeable_share_root(sandbox_path)
}
