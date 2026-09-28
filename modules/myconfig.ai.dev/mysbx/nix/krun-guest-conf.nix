# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The nested-podman guest configuration of the podman-krun backend
# (bd myconfig-6di.5.8, docs/design/backends.md D2): one store tree
# carrying the `/etc/containers/{containers,storage}.conf` podman
# reads, plus `/etc/subuid`/`/etc/subgid` for the guest-root user's
# subordinate ranges. Baked into the (shared) agent image via
# `myconfig.ai.dev.mysbx.krun.nestedPodman.packages` — the image's
# `/etc` already exists (dockerTools.caCertificates, fakeNss), and
# buildEnv links this tree's `etc` into it.
#
# The values are the verified-consequence set of the krun guest model:
#
# - `events_logger = "file"` — no journald inside the guest (the guest
#   init is not systemd; journald does not exist), podman must not
#   warn-and-fallback on every invocation.
# - `cgroup_manager = "cgroupfs"` + `cgroups = "disabled"` — the guest
#   has a cgroup2 mount (libkrun init mount_filesystems) but nothing
#   delegates it and no controller wiring for a nested pod exists;
#   cgroupfs + disabled keeps podman from dying on a systemd DBus that
#   does not exist, while `--cgroup-manager=cgroupfs` stays the
#   documented per-invocation override.
# - `network_cmd_path` unset, `no_pivot_root = true` — the guest runs
#   everything as root in one mount namespace handed over virtio-fs;
#   pivot_root on the virtiofs root is exactly what the guest kernel
#   does NOT support cleanly (the shared tree is the VM's root, and
#   the upper-layer question is bd myconfig-6di.5.9's).
# - storage.conf: `driver = "overlay"` with EXPLICIT
#   `graphroot = "/var/tmp/containers/storage"` and
#   `runroot = "/run/containers/storage"` — the podman defaults
#   (`/var/lib/containers/storage`) would sit on the READ-ONLY
#   virtiofs root (podman's `--read-only-tmpfs` tmpfses only
#   `/dev, /dev/shm, /run, /tmp, /var/tmp` — cmd/podman/common/
#   create.go), so both roots are pinned onto the tmpfs surfaces
#   the container already has. Costs VM RAM (tmpfs), which the krun
#   limit pins can size (bd myconfig-6di.5.6); a host needing bigger
#   storage mounts a tmpfs or disk over the paths. `mount_program`
#   stays unset: the overlay driver needs the guest kernel's overlayfs
#   (libkrunfw has OVERLAY_FS) — fuse-overlayfs (already on the
#   wrapped podman's PATH) is the fallback only if that proves broken
#   in live validation (bd myconfig-6di.5.7).
# - subuid/subgid: a subordinate range for the guest-root user
#   (`agent`) — root inside the guest maps its inner containers
#   through it. The HOST user's own ranges do not apply here: the
#   guest's uid space is the VM's, the virtiofs mapping to the host
#   user happens below it (backends.md D2, the uid-mapping row).
{
  runCommand,
  subUidRange ? "100000:65536",
  containersConf ? null,
  storageConf ? null,
}:
let
  defaultContainersConf = ''
    [containers]
    # No journald inside the guest — file logging, never warn-and-fallback.
    apparmor_profile = ""
    log_driver = "k8s-file"

    [engine]
    cgroup_manager = "cgroupfs"
    cgroups = "disabled"
    events_logger = "file"
    # The virtiofs root is the VM's own root: no pivot_root on it.
    no_pivot_root = true
  '';

  defaultStorageConf = ''
    [storage]
    driver = "overlay"
    # The guest root is READ-ONLY virtio-fs and podman's
    # --read-only-tmpfs covers only /dev, /dev/shm, /run, /tmp,
    # /var/tmp — so both roots are pinned onto tmpfs surfaces
    # (the defaults would sit on the read-only root and every image
    # pull would die with EROFS).
    graphroot = "/var/tmp/containers/storage"
    runroot = "/run/containers/storage"
  '';
in
runCommand "mysbx-krun-guest-conf"
  {
    containersConfText = if containersConf == null then defaultContainersConf else containersConf;
    storageConfText = if storageConf == null then defaultStorageConf else storageConf;
  }
  ''
    mkdir -p $out/etc/containers
    printf '%s\n' "$containersConfText" > $out/etc/containers/containers.conf
    printf '%s\n' "$storageConfText" > $out/etc/containers/storage.conf
    # The registries defaults of the image apply; nothing to pin here
    # yet (pull policy is the acceptance runbook's variable, .7).
    printf 'agent:%s\n' "${subUidRange}" > $out/etc/subuid
    printf 'agent:%s\n' "${subUidRange}" > $out/etc/subgid
  ''
