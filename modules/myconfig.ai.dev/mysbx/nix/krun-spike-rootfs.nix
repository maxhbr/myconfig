# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The spike rootfs of the direct-libkrun backend (bd myconfig-dak.1):
# a PLAIN DIRECTORY built by Nix, no OCI image, no tar. libkrun shares
# it read-only over virtiofs (KRUN_FS_ROOT_TAG) and boots it with its
# implicit /init.krun, which mounts devtmpfs/proc/sysfs and execs the
# payload configured by krun_set_exec — for the spike that is
# bin/spike-init below.
#
# What is inside and why:
#
# - bin/spike-init: the guest entry (bd myconfig-dak.5's precursor).
#   libkrun's implicit init mounts NO extra virtiofs tags — the guest
#   kernel exposes each tag as a virtiofs device, and someone inside
#   must mount it. spike-init mounts each share the launcher hands
#   over (the MYSBX_KRUN_SHARES encoding of the --ro-share/--rw-share
#   flags, see ../krun-rs), then execs the payload of
#   MYSBX_KRUN_PAYLOAD from the same environment krun_set_exec set.
# - bin/bash, bin/sh: real symlinks into the store, so spike-init's
#   own shebang resolves inside the ro rootfs.
# - nix/store, dev, proc, sys, tmp: EMPTY directories — mountpoints.
#   The launcher shares the host /nix/store read-only as a second
#   virtiofs tag and spike-init mounts it over nix/store, so a payload
#   runs a toolchain STRAIGHT from the host store without any image
#   bake — the epic's core claim (bd myconfig-dak.2's option (a)).
#   Everything else the payload runs resolves through that share; the
#   rootfs itself stays a few symlinks big.
#
# The spike runbook (nix/krun-direct-spike.sh) builds this tree and
# the launcher, wraps the launcher in bwrap with only /dev/kvm, this
# rootfs, /nix/store and the payload directory visible, and times the
# boot against podman-krun.
{
  runCommand,
  bash,
}:
let
  # The guest entry script. It runs as the first process of the
  # payload (after /init.krun forked), as root, in the VM. Mounting
  # is therefore always permitted; failures are exit 125 (libkrun's
  # own "init cannot set up the environment" code, so a failure
  # reaches the caller as an infrastructure error, not a payload one).
  spikeInit = ''
    #!/bin/sh
    set -eu

    fail() {
        echo "spike-init: $*" >&2
        exit 125
    }

    # Each launcher --ro-share/--rw-share TAG@DEST=HOSTDIR becomes one
    # "TAG DEST ro|rw" line here; the launcher owns the encoding, this
    # script only consumes it.
    shares="''${MYSBX_KRUN_SHARES:-}"
    while [ -n "$shares" ]; do
        entry=''${shares%%';'*}
        [ "$shares" = "$entry" ] && shares= || shares=''${shares#*';'}
        [ -n "$entry" ] || continue
        tag=''${entry%%' '*}
        rest=''${entry#*' '}
        dest=''${rest%%' '*}
        mode=''${rest#*' '}
        mkdir -p "$dest" \
            || fail "cannot create the mountpoint $dest"
        if [ "$mode" = ro ]; then
            mount -t virtiofs -o ro "$tag" "$dest" \
                || fail "cannot mount the virtiofs tag $tag at $dest (ro)"
        else
            mount -t virtiofs "$tag" "$dest" \
                || fail "cannot mount the virtiofs tag $tag at $dest (rw)"
        fi
    done

    # The toolchain-from-the-host-store proof: nothing was baked into
    # this rootfs beyond symlinks; every binary the payload runs
    # resolves through the read-only /nix/store share.
    exec "''${MYSBX_KRUN_PAYLOAD:?spike-init needs MYSBX_KRUN_PAYLOAD}"
  '';
in
runCommand "mysbx-krun-spike-rootfs"
  {
    passthru.spikeInit = spikeInit;
    spikeInitText = spikeInit;
  }
  ''
    mkdir -p $out/bin $out/nix/store $out/dev $out/proc $out/sys $out/tmp
    ln -s ${bash}/bin/bash $out/bin/bash
    ln -s ${bash}/bin/sh $out/bin/sh
    printf '%s' "$spikeInitText" > $out/bin/spike-init
    chmod +x $out/bin/spike-init
  ''
