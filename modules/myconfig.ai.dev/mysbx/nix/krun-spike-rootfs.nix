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
#   must mount it. spike-init mounts a guest tmpfs on /tmp (the ro
#   root cannot take the share mountpoints below it — EROFS), mounts
#   each share the launcher hands over (the MYSBX_KRUN_SHARES encoding
#   of the --ro-share/--rw-share flags, see ../krun-rs), then execs
#   its own argv ($1..: krun_set_exec's payload vector — /init.krun
#   overwrites the vector's argv[0] with KRUN_INIT and forwards the
#   rest unmodified). Its shebang is the STATIC busybox sh, and every builtin
#   it uses is a busybox applet: the entry must run BEFORE the host
#   /nix/store share is mounted, so a store-symlinked shell would
#   dangle (the first live spike run died exactly there).
# - bin/busybox: the static multi-call binary, carrying sh, mount,
#   mkdir, touch — the whole entry path, self-contained.
# - bin/bash, bin/sh: store symlinks for the PAYLOAD (bash needs its
#   libc on the store share, which spike-init has just mounted).
# - nix/store, dev, proc, sys, tmp: EMPTY directories — mountpoints.
#   The launcher shares the host /nix/store read-only as a second
#   virtiofs tag and spike-init mounts it over nix/store, so a payload
#   runs a toolchain STRAIGHT from the host store without any image
#   bake — the epic's core claim (bd myconfig-dak.2's option (a)).
#
# The spike runbook (nix/krun-direct-spike.sh) builds this tree and
# the launcher, wraps the launcher in bwrap with only /dev/kvm, this
# rootfs, /nix/store and the payload directory visible, and times the
# boot against podman-krun.
{
  runCommand,
  # The STATIC busybox of the guest entry: pkgsStatic so spike-init
  # and every applet it invokes run without any store path — the
  # dynamic bash below is for the payload only, usable only after the
  # store share is mounted.
  busyboxStatic,
  bash,
}:
let
  # The guest entry script. It runs as the first process of the
  # payload (after /init.krun forked), as root, in the VM: /init.krun
  # replaces its own argv[0] with KRUN_INIT and execvp()s it with the
  # krun_set_exec argv vector — the payload path arrives as $1, its
  # arguments as $2.. Mounting is always permitted; failures are exit
  # 125 (libkrun's own "init cannot set up the environment" code, so
  # a failure reaches the caller as an infrastructure error, not a
  # payload one). POSIX sh ONLY — the busybox-static shell, no
  # bashisms: the script must survive without the store share
  # mounted.
  spikeInit = ''
    #!/bin/busybox sh
    set -eu

    fail() {
        echo "spike-init: $*" >&2
        exit 125
    }

    # The guest's writable scratch (bd myconfig-dak.5's "tmpfs home"):
    # the root is a READ-ONLY virtiofs share, so every share
    # mountpoint below /tmp would die with EROFS without this.
    mount -t tmpfs tmpfs /tmp \
        || fail "cannot mount the guest tmpfs on /tmp"

    # Each launcher --ro-share/--rw-share TAG@DEST=HOSTDIR becomes one
    # "TAG DEST ro|rw" entry here; the launcher owns the encoding,
    # this script only consumes it. Only busybox applets from here on
    # (mkdir, mount) — the store is not visible yet.
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
    # this rootfs beyond the static busybox and symlinks; every
    # binary the payload runs resolves through the read-only
    # /nix/store share mounted above. The payload is our own argv:
    # $1 the path (absolute, no PATH lookup needed), $2.. its
    # arguments, exactly as krun_set_exec carried them.
    [ "$#" -gt 0 ] || fail "no payload in argv — the krun_set_exec args did not reach the guest init"
    shift
    exec "$@"
  '';
in
runCommand "mysbx-krun-spike-rootfs"
  {
    passthru.spikeInit = spikeInit;
    spikeInitText = spikeInit;
  }
  ''
    mkdir -p $out/bin $out/nix/store $out/dev $out/proc $out/sys $out/tmp
    # The entry path: STATIC busybox (sh, mkdir, mount), runnable
    # with nothing else mounted. A real copy, not a symlink: the
    # store share covers $out/nix/store only, but the file must also
    # survive a launcher run that shares NO store tag.
    cp ${busyboxStatic}/bin/busybox $out/bin/busybox
    # The payload's shells: store symlinks, valid once spike-init has
    # mounted the store share.
    ln -s ${bash}/bin/bash $out/bin/bash
    ln -s ${bash}/bin/sh $out/bin/sh
    printf '%s' "$spikeInitText" > $out/bin/spike-init
    chmod +x $out/bin/spike-init
  ''
