# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Direct-libkrun spike rootfs: a plain directory shared read-only over
# virtiofs. The static BusyBox entry mounts /tmp and configured shares,
# then executes the payload; payload shells are store symlinks usable
# after the /nix/store share is mounted.
#
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

    # No PATH in the guest env (the launcher passes an explicit envp
    # only) and nothing else is mounted — every applet is invoked
    # through the static busybox at $BB, never by bare name.
    BB=/bin/busybox

    fail() {
        echo "spike-init: $*" >&2
        exit 125
    }

    # The guest's writable scratch (bd myconfig-dak.5's "tmpfs home"):
    # the root is a READ-ONLY virtiofs share, so every share
    # mountpoint below /tmp would die with EROFS without this.
    "$BB" mount -t tmpfs tmpfs /tmp \
        || fail "cannot mount the guest tmpfs on /tmp"

    # Each launcher --ro-share/--rw-share TAG@DEST=HOSTDIR becomes one
    # "TAG DEST ro|rw" entry here; the launcher owns the encoding,
    # this script only consumes it. Only busybox applets from here on
    # (mkdir, mount), each invoked through the static busybox at
    # /bin/busybox: the guest env carries no PATH, a bare applet name
    # is unresolvable there.
    shares="''${MYSBX_KRUN_SHARES:-}"
    while [ -n "$shares" ]; do
        entry=''${shares%%';'*}
        [ "$shares" = "$entry" ] && shares= || shares=''${shares#*';'}
        [ -n "$entry" ] || continue
        tag=''${entry%%' '*}
        rest=''${entry#*' '}
        dest=''${rest%%' '*}
        mode=''${rest#*' '}
        "$BB" mkdir -p "$dest" \
            || fail "cannot create the mountpoint $dest"
        if [ "$mode" = ro ]; then
            "$BB" mount -t virtiofs -o ro "$tag" "$dest" \
                || fail "cannot mount the virtiofs tag $tag at $dest (ro)"
        else
            "$BB" mount -t virtiofs "$tag" "$dest" \
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
