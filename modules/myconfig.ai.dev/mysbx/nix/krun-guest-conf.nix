# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The nested-podman guest userspace of the podman-krun backend
# (bd myconfig-6di.5.8, docs/design/backends.md D2): one store tree
# carrying `bin/podman` (the storage wrapper below, which execs the
# real `podman`), the `/etc/containers/{containers,storage}.conf` and
# `policy.json` podman reads, plus `/etc/subuid`/`/etc/subgid` for the
# guest-root user's subordinate ranges. Baked into the (shared) agent
# image via `myconfig.ai.dev.mysbx.krun.nestedPodman.packages` — the
# image's `/etc` already exists (dockerTools.caCertificates, fakeNss),
# and buildEnv links this tree's `bin` and `etc` into it.
#
# The values are the verified-consequence set of the krun guest model:
#
# - `events_logger = "file"` — no journald inside the guest (the guest
#   init is not systemd; journald does not exist), podman must not
#   warn-and-fallback on every invocation.
# - `[engine] cgroup_manager = "cgroupfs"` + `[containers] cgroups =
#   "disabled"` (podman reads each key ONLY from its own table and
#   silently ignores it anywhere else) — the guest has a cgroup2
#   mount (libkrun init mount_filesystems) but nothing delegates it
#   and no controller wiring for a nested pod exists;
#   cgroupfs + disabled keeps podman from dying on a systemd DBus that
#   does not exist, while `--cgroup-manager=cgroupfs` stays the
#   documented per-invocation override.
# - `[containers] netns = "host"` — nested containers share the guest's
#   one network stack instead of podman's rootful default (a netavark
#   bridge + NAT). The guest has no NIC, only loopback: crun sets neither
#   `krun.tap_name` nor `krun.use_passt`, so all egress is libkrun's
#   TSI socket hijack (backends.md D2, the network model). A bridged
#   container's forwarded packets would have no egress interface, and
#   netavark's iptables driver would find no xtables in libkrunfw
#   (NETFILTER_XTABLES and IP_NF_IPTABLES unset). pasta would add a
#   tun device and a second forwarder in front of the same TSI path.
#   Cost: nested containers see the guest's loopback services — the
#   guest itself is the sandbox boundary, and mysbx's `network = false`
#   holds unchanged (the VMM has no route). `podman run --network=...`
#   still overrides per invocation; `podman build` does not read this
#   key, so RUN steps that need the network take `--network=host`.
# - `no_pivot_root = true` — the guest runs
#   everything as root in one mount namespace handed over virtio-fs;
#   pivot_root on the virtiofs root is exactly what the guest kernel
#   does NOT support cleanly (the shared tree is the VM's root).
# - all podman state on GUEST-native tmpfs (bd myconfig-6di.5.16,
#   backends.md D2 for the virtio-fs reasons): as guest root,
#   `bin/podman` mounts tmpfs at `graphMount` and `runMount` (once
#   per VM, under a lock on the guest's /dev/shm) and then execs the
#   real podman; a failed mount is exit 125, never a fallback. The
#   conf keeps graphroot, runroot, tmp_dir, image_copy_tmp_dir
#   ("storage": graphroot/tmp, unless TMPDIR is set), the network
#   config dir and crun's state root below those two mounts. As any
#   other uid the wrapper execs podman untouched. Each tmpfs may
#   take up to half the VM RAM (tmpfs default; krun limit pins, bd
#   myconfig-6di.5.6) — large images need a larger VM. `driver =
#   "overlay"` uses the guest kernel's overlayfs (libkrunfw
#   OVERLAY_FS); fuse-overlayfs (on podman's PATH) is the fallback
#   if that fails live (bd myconfig-6di.5.7). Overrides of
#   `storageConf`/`containersConf` must keep these paths below the
#   mounts.
# - policy.json: containers/image refuses every pull when neither
#   `/etc/containers/policy.json` nor the user's
#   `~/.config/containers/policy.json` exists, and neither pkgs.podman
#   nor the agent image ships one (on NixOS it comes from the
#   virtualisation.containers module, not the package). The default
#   is that module's default, skopeo's default-policy.json:
#   insecureAcceptAnything — no signature verification, the same
#   trust a plain NixOS podman host has. `containersPolicy` overrides
#   it.
# - subuid/subgid: a subordinate range for the guest-root user
#   (`agent`) — root inside the guest maps its inner containers
#   through it. The HOST user's own ranges do not apply here: the
#   guest's uid space is the VM's, the virtiofs mapping to the host
#   user happens below it (backends.md D2, the uid-mapping row).
{
  lib,
  runCommand,
  writeShellApplication,
  coreutils,
  util-linux,
  podman,
  graphMount ? "/var/tmp/containers",
  runMount ? "/run/containers",
  subUidRange ? "100000:65536",
  containersConf ? null,
  storageConf ? null,
  containersPolicy ? null,
}:
let
  defaultContainersConf = ''
    [containers]
    apparmor_profile = ""
    # Nothing delegates the guest's cgroup2 mount.
    cgroups = "disabled"
    # No journald inside the guest — file logging, never warn-and-fallback.
    log_driver = "k8s-file"
    # The guest's only network is TSI; no bridge, no NAT, no firewall.
    netns = "host"

    [engine]
    cgroup_manager = "cgroupfs"
    events_logger = "file"
    # The virtiofs root is the VM's own root: no pivot_root on it.
    no_pivot_root = true
    # Must be tmpfs: the guest tmpfs the podman wrapper mounts.
    tmp_dir = "${runMount}/libpod"
    # Pull/build/load temp data in graphroot/tmp, not on virtio-fs /var/tmp.
    image_copy_tmp_dir = "storage"

    [engine.runtimes_flags]
    # crun's state (status, exec fifo) off the virtio-fs /run/crun.
    crun = ["root=${runMount}/crun"]

    [network]
    # The default /etc/containers/networks sits on the read-only root.
    network_config_dir = "${graphMount}/networks"
  '';

  defaultStorageConf = ''
    [storage]
    driver = "overlay"
    # Guest-native tmpfs mounts, made by the podman wrapper: container
    # storage cannot live on the virtio-fs share (chown, uids, xattrs).
    graphroot = "${graphMount}/storage"
    runroot = "${runMount}/storage"
  '';

  wrapper = writeShellApplication {
    name = "podman";
    runtimeInputs = [
      coreutils
      util-linux
    ];
    text = ''
      if [ "$(id -u)" -eq 0 ]; then
        fail() {
          echo "podman (mysbx krun wrapper): $*" >&2
          echo "  nested podman storage must not live on the virtio-fs share (docs/design/backends.md D2)" >&2
          exit 125
        }
        # A directory lock on the guest's own tmpfs: creates nothing, and
        # keeps two first invocations from stacking tmpfs mounts.
        { exec 9</dev/shm; } 2>/dev/null || fail "cannot open /dev/shm for the storage lock"
        flock -w 60 9 || fail "cannot take the storage lock on /dev/shm"
        for dir in ${lib.escapeShellArg graphMount} ${lib.escapeShellArg runMount}; do
          if ! findmnt -rn -t tmpfs --mountpoint "$dir" >/dev/null; then
            { mkdir -p "$dir" && mount -t tmpfs -o mode=0755 mysbx-podman-storage "$dir"; } \
              || fail "cannot mount a guest tmpfs at $dir"
          fi
        done
        # Podman's image-copy temp dir lands on the FIRST pull, before
        # containers/storage creates the store: `image_copy_tmp_dir =
        # "storage"` resolves to graphroot/tmp, and os.MkdirTemp fails
        # with ENoent when the parent does not exist yet. The wrapper
        # owns the guest state, so it pre-creates the temp parents.
        mkdir -p "${graphMount}/storage/tmp" "${runMount}/libpod" "${runMount}/crun" \
          || fail "cannot create the podman state directories on the guest tmpfs"
        exec 9<&-
      fi
      exec ${podman}/bin/podman "$@"
    '';
  };

  defaultPolicy = {
    default = [ { type = "insecureAcceptAnything"; } ];
    transports.docker-daemon."" = [ { type = "insecureAcceptAnything"; } ];
  };
in
runCommand "mysbx-krun-guest-conf"
  {
    passthru = { inherit podman wrapper; };
    containersConfText = if containersConf == null then defaultContainersConf else containersConf;
    storageConfText = if storageConf == null then defaultStorageConf else storageConf;
    policyText = builtins.toJSON (if containersPolicy == null then defaultPolicy else containersPolicy);
  }
  ''
    mkdir -p $out/bin $out/etc/containers
    ln -s ${wrapper}/bin/podman $out/bin/podman
    printf '%s\n' "$containersConfText" > $out/etc/containers/containers.conf
    printf '%s\n' "$storageConfText" > $out/etc/containers/storage.conf
    printf '%s\n' "$policyText" > $out/etc/containers/policy.json
    # The registries defaults of the image apply; nothing to pin here
    # yet (pull policy is the acceptance runbook's variable, .7).
    printf 'agent:%s\n' "${subUidRange}" > $out/etc/subuid
    printf 'agent:%s\n' "${subUidRange}" > $out/etc/subgid
  ''
