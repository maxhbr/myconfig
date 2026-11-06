# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The krun guest rootfs of the direct-libkrun backend
# (docs/design/backends.md D3, bd myconfig-dak.4): a PLAIN DIRECTORY
# built by Nix — no OCI image, no tar — shared read-only over virtiofs
# as KRUN_FS_ROOT_TAG (krun.rs passes its store path as --rootfs).
#
# What is baked and why (D3's rootfs decision — the root is a
# READ-ONLY virtiofs share, so NOTHING the guest needs to modify may
# live as a real entry on it):
#
# - bin/mysbx-init: the guest entry (bd myconfig-dak.5; the script
#   itself lives in mysbx-init.nix). Its shebang is the STATIC
#   busybox sh and every applet it invokes goes through
#   /bin/busybox: the entry runs BEFORE the store share is mounted,
#   so a store-symlinked shell would dangle (the spike's first live
#   run died exactly there, bd myconfig-dak.1 finding 10).
# - bin/busybox: the static multi-call binary (sh, mount, mkdir,
#   ln, rm — the whole entry path, self-contained).
# - bin/bash, bin/sh: store symlinks for the PAYLOAD, valid once
#   mysbx-init has mounted the store share.
# - nix/store: a symlink to /tmp/mysbx-shares/store — the store
#   share's SANDBOX path, resolved through the guest tmpfs (a second
#   virtiofs device cannot nest below the root share, EBUSY — spike
#   finding 9; the baked link is the only writable-root-safe way to
#   put the share at its contract path).
# - mysbx-home: a symlink to /tmp/mysbx-home — the tmpfs home the
#   init builds (the XDG parents, the mux socket dir, the state
#   share links) lives on the /tmp tmpfs, and the baked link keeps
#   /mysbx-home (config.md D14's path) reachable from the ro root.
# - dev, proc, sys, tmp: EMPTY directories — mountpoints of the
#   guest's own filesystems (libkrun's implicit init mounts devtmpfs,
#   proc and sysfs over the first three; /tmp becomes the guest
#   tmpfs).
#
# Nothing else: every toolchain, shell and tool the payload runs
# resolves through the read-only store share (D3's core claim). One
# rootfs therefore serves every repo and every toolchain set.
{
  runCommand,
  lib,
  # The STATIC busybox of the guest entry: pkgsStatic so mysbx-init
  # and every applet it invokes run without any store path (the
  # dynamic bash below is for the payload only, usable only after the
  # store share is mounted).
  busyboxStatic,
  # The coreutils `env` of the `/usr/bin/env` shebangs: a store
  # symlink like bin/bash — a repo script's `#!/usr/bin/env bash`
  # must resolve once the store share is mounted, and no guest
  # component provides `/usr/bin` (the bwrap tier binds the host's;
  # the podman image bakes dockerTools.usrBinEnv). The rootfs bakes
  # the same contract the base table's `/usr/bin` row names
  # (docs/plan.md, base table).
  coreutils,
  # The STATIC mkfs.ext4 of the scratch half (bd myconfig-dak.7,
  # backends.md D7): the init formats the per-run virtio-blk device
  # BEFORE any store path is visible — pkgsStatic, like busybox.
  e2fsprogsStatic,
  # The STATIC sqlite3 of the db reconcile (bd myconfig-mxu): the
  # init runs before any store path is visible, yet must prune the
  # copied host db of rows whose store path the staged share no
  # longer carries (host rebuild restage) or carries as a symlink
  # (the flake-source poisoning, doc/TODOs/
  # krun-guest-flake-source-symlink.md). A real copy, like busybox
  # and mkfs.ext4 — pkgsStatic so it works store-free.
  sqliteStatic,
  # The STATIC bulk directory scanner of the same reconcile: one
  # getdents pass classifies every share entry (the d_type arrives
  # in the readdir stream), instead of one virtiofs FUSE round trip
  # per registered path — ~175k paths made the boot minute-slow.
  # Same real-copy contract as busybox (nix/krun-store-scan.nix).
  storeScanStatic,
  python3,
  bash,
}:
let
  # The guest entry script (bd myconfig-dak.5) — the assembly of the
  # functional parts in ./mysbx-init/ (one file per phase, concatenated
  # IN ORDER into the ONE script the guest runs; see mysbx-init/default.nix
  # for why the parts are source modules, not guest scripts). The two
  # interpolations the fragments carry are generated HERE because the
  # indented-string '' rules make inlining them error-prone: the
  # shell-quoted reconcile SQL, and the `#` of the step timer's `10#`
  # base prefix (a bare `#` would open a Nix comment and a bare `${`
  # would interpolate inside the string).
  # The assembly is byte-identical by contract: passthru.mysbxInit
  # and the baked bin/mysbx-init must keep producing the SAME bytes.
  mysbxInit =
    (import ./mysbx-init {
      basePrefix = "#";
      # The db reconcile's SQL, shell-quoted by Nix (the init text
      # interpolates it verbatim): the pattern needs SQL single
      # quotes. A store path's name carries no quote, so no escaping
      # of VALUES is needed (only the LIKE pattern here is a literal).
      reconcileSelect = lib.strings.escapeShellArg "SELECT path FROM ValidPaths WHERE path LIKE '/nix/store/%'";
    }).text;
in
runCommand "mysbx-krun-rootfs"
  {
    passthru.mysbxInit = mysbxInit;
    # The init text as a derivation attr, never interpolated into
    # the builder text: the script carries double quotes and
    # $(...), so bash would re-parse it. The env-var route hands the
    # bytes through verbatim (the spike rootfs's own pattern).
    initText = mysbxInit;
    nativeBuildInputs = [ python3 ];
  }
  ''
    mkdir -p $out/bin $out/dev $out/proc $out/sys $out/tmp $out/nix
    # The share-root mountpoints (the crate's BAKED_SHARE_ROOTS —
    # keep the two lists identical): the guest init places a share
    # below one of these by mounting a tmpfs OVER the component,
    # which needs the mountpoint on the ro root; the builder refuses
    # a dest below anything else (never a run-time ENOENT).
    mkdir -p $out/etc $out/home $out/srv $out/mnt $out/media $out/opt $out/data
    # The localhost → 127.0.0.1 mapping: the guest has no DHCP, no
    # nsswitch beyond the busybox defaults, and no other source of
    # this mapping. Without it Node.js fetch('http://localhost:…')
    # fails with getaddrinfo ENOTFOUND (curl resolves localhost itself
    # and masked the gap). The init copies this file to the /etc
    # tmpfs when it mounts one for resolv.conf.
    printf '127.0.0.1 localhost\n::1 localhost\n' > $out/etc/hosts
    # The uid the payload runs as (bd myconfig-v3fk, the live run's
    # finding): the ro root bakes no passwd, and openssh's ssh-keygen
    # refuses an unnamed uid ("No user exists for uid …") — the
    # sandbox keypair step died on it, and the tmux multiplexers each
    # carry their own SHELL pin because tmux cannot resolve the
    # pane shell either. root + nobody is the honest minimal set:
    # the host user's own name cannot be baked (the rootfs is
    # host-independent), and the payload runs as uid 0 in the VM.
    # The init copies both to the /etc tmpfs when it mounts one,
    # the same carry-over rule as hosts above.
    printf 'root:x:0:0:root:/root:/bin/sh\nnobody:x:65534:65534:nobody:/nonexistent:/bin/false\n' > $out/etc/passwd
    printf 'root:x:0:\nnobody:x:65534:\n' > $out/etc/group
    # The scratch disk's mountpoint (bd myconfig-dak.7, backends.md
    # D7): the ro root can hold no new entries at run time, so the
    # ext4's target is baked here — the live run's finding (mkdir
    # EROFS).
    mkdir -p $out/mysbx-nix
    # The baked links of the fixed sandbox paths (the ro root can
    # hold no new entries at run time): the store share's and the
    # tmpfs home's contract paths.
    ln -s /tmp/mysbx-shares/stage-ro/store $out/nix/store
    ln -s /tmp/mysbx-home $out/mysbx-home
    # The entry path: STATIC busybox (sh, mount, mkdir, ln, rm),
    # runnable with nothing else mounted. A real copy, not a symlink:
    # the store share covers $out/nix/store only, but the file must
    # also survive a launcher run that shares NO store tag.
    cp ${busyboxStatic}/bin/busybox $out/bin/busybox
    # The scratch disk's formatter (bd myconfig-dak.7): a REAL copy
    # like busybox itself — it must run before any store share is
    # mounted.
    cp ${e2fsprogsStatic}/sbin/mkfs.ext4 $out/bin/mkfs.ext4
    # The db reconcile's static sqlite3 (bd myconfig-mxu): same
    # real-copy contract — the init runs it before the store share
    # is mounted.
    cp ${sqliteStatic}/bin/sqlite3 $out/bin/sqlite3
    # The db reconcile's static store scanner (bd myconfig-kid): same
    # real-copy contract — the bulk getdents pass runs before the
    # store share is overlaid.
    cp ${storeScanStatic}/bin/store-scan $out/bin/store-scan
    # The payload's shells: store symlinks, valid once mysbx-init has
    # mounted the store share.
    ln -s ${bash}/bin/bash $out/bin/bash
    ln -s ${bash}/bin/sh $out/bin/sh
    # The `/usr/bin/env` shebangs (docs/plan.md, base table row
    # `/usr/bin` ro): same store-symlink contract as the shells.
    mkdir -p $out/usr/bin
    ln -s ${coreutils}/bin/env $out/usr/bin/env
    printf '%s' "$initText" > $out/bin/mysbx-init
    chmod +x $out/bin/mysbx-init
    python3 ${./krun-rootfs-test.py} "$out"
    grep -qF 'cannot enumerate registered store paths' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init does not check the sqlite query status" >&2; exit 1; }
    grep -qF 'PRAGMA foreign_keys=ON' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init does not enable sqlite foreign keys" >&2; exit 1; }
    grep -qF 'DELETE FROM Refs WHERE reference =' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init does not remove surviving referrers" >&2; exit 1; }
    grep -qF 'PRAGMA foreign_key_check;' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init does not verify sqlite foreign keys" >&2; exit 1; }
    # The degradation probe (bd myconfig-mxu): the init must refuse
    # a payload whose baked store symlinks dangle — a launch-time
    # diagnosis instead of the live mid-session ENOENT.
    grep -qF 'the store share lost' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init lost the store-share degradation probe" >&2; exit 1; }
    # The qemu exit channel (backends.md D9): the init must report
    # the payload's exit code on the virtio-serial port the launcher
    # reads — resolved by NAME through sysfs (bd myconfig-dnmk: the
    # /dev/virtio-ports/<name> spelling is a udev symlink this
    # static-busybox guest never has; the node is devtmpfs's own
    # vport<pdev>p<id>).
    grep -qF '/sys/class/virtio-ports' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init lost the exit-channel sysfs resolution" >&2; exit 1; }
    grep -qF 'mknod' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init lost the exit-channel node fallback" >&2; exit 1; }
    # The payload's controlling terminal (bd myconfig-w8jd): without
    # setsid -c the payload child never acquires the console —
    # interactive runs have no job control and /dev/tty is ENXIO.
    grep -qF 'setsid -c' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init lost the payload ctty acquisition" >&2; exit 1; }
    # The manifest argv records (bd myconfig-hq7k): the payload rides
    # the manifest on the qemu backend — the init must both CONSUME
    # the records and refuse a corrupted one.
    grep -qF 'manifest argv index' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init lost the manifest argv loader" >&2; exit 1; }
    grep -qF 'cannot decode the manifest argv' $out/bin/mysbx-init \
        || { echo "mysbx-krun-rootfs: init lost the manifest argv decode refusal" >&2; exit 1; }
    # The shebang guard (the eighth live finding): an indented
    # Nix string strips only the MINIMAL common indent of its
    # lines, so mixed indents leave leading spaces before the
    # shebang — the kernel then refuses it, the execvp ENOEXEC
    # fallback chases /bin/sh (a dangling store symlink at that
    # point), and the guest dies with a bare 127. The build
    # refuses that init.
    [ "$(head -c 2 $out/bin/mysbx-init)" = "#!" ] \
        || { echo "mysbx-krun-rootfs: the init lost its shebang (mixed indent?)" >&2; exit 1; }
  ''
