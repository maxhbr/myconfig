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
# - bin/mysbx-init: the guest entry (bd myconfig-dak.5). Its shebang
#   is the STATIC busybox sh and every applet it invokes goes through
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
  # The STATIC busybox of the guest entry: pkgsStatic so mysbx-init
  # and every applet it invokes run without any store path (the
  # dynamic bash below is for the payload only, usable only after the
  # store share is mounted).
  busyboxStatic,
  bash,
}:
let
  # The guest entry script (bd myconfig-dak.5). It runs as the first
  # process of the payload (after /init.krun forked), as root, in the
  # VM: /init.krun replaces its own argv[0] with KRUN_INIT and
  # execvp()s it with the krun_set_exec argv vector — the payload
  # path arrives as $1, its arguments as $2.. The launcher's explicit
  # envp carries MYSBX_KRUN_SHARES ("DEVICE SLOT SANDBOX_PATH
  # ro|rw;..." — bd myconfig-xpq's grouped devices).
  # Failures exit 125 (libkrun's own "init cannot set up the
  # environment" code, so a failure reaches the caller as an
  # infrastructure error, not a payload one). POSIX sh ONLY — the
  # busybox-static shell, no bashisms: the script must survive
  # without the store share mounted.
  mysbxInit = ''
    #!/bin/busybox sh
    set -eu

    # No PATH in the guest env (the launcher passes an explicit envp
    # only) and nothing else is mounted — every applet is invoked
    # through the static busybox at $BB, never by bare name.
    BB=/bin/busybox

    fail() {
        echo "mysbx-init: $*" >&2
        exit 125
    }

    # The guest's writable scratch: the root is a READ-ONLY virtiofs
    # share, so nothing below / may change — /tmp is where every
    # writable thing lives, the share mounts included (finding 9:
    # a second virtiofs device cannot nest below the root share,
    # EBUSY).
    step "mounting the guest tmpfs on /tmp"
    "$BB" mount -t tmpfs tmpfs /tmp \
        || fail "cannot mount the guest tmpfs on /tmp"
    "$BB" mkdir -p /tmp/mysbx-shares /tmp/mysbx-home \
        || fail "cannot create the tmpfs roots"
    # The sandbox home's tree (config.md D14): the baked /mysbx-home
    # symlink resolves here. The XDG parents exist for tools that
    # write before reading $XDG_* (the payload env names these paths);
    # the mux socket dir is the same MUX_SOCKET_DIR the env's
    # TMUX_TMPDIR names (config.md D16/D17) — tmpfs, so guest-internal
    # sockets work and never touch a host-shared surface.
    "$BB" mkdir -p /tmp/mysbx-home/.local/share /tmp/mysbx-home/.local/state \
        /tmp/mysbx-home/.config /tmp/mysbx-home/.cache \
        /tmp/mysbx-home/.mysbx-tmux \
        || fail "cannot create the sandbox home tree"

    # A tmpfs at the FIRST component of a sandbox path outside /tmp
    # and the home: the ro root can hold no new entries, but mounting
    # a tmpfs OVER a root-share subdirectory is fine (libkrun's own
    # implicit init mounts devtmpfs/proc/sysfs exactly that way; the
    # EBUSY of finding 9 is virtiofs-on-virtiofs only). The first
    # component of a share's sandbox path carries nothing but the
    # rootfs skeleton below it, so covering it costs no content —
    # and the share's link then lives on writable tmpfs. Mounted
    # once per component, shared by every share under it.
    tmpfs_first_components=""
    mounted_devices=""

    # Each share: the launcher encoded one "DEVICE SLOT SANDBOX_PATH
    # ro|rw" entry per --ro-share/--rw-share (bd myconfig-xpq: the
    # DEVICE count is the virtiofs slot budget, so shares group into
    # staged devices; the tag names the virtiofs device the guest
    # kernel exposed). Each device is mounted ONCE under
    # /tmp/mysbx-shares keyed by its tag; the SLOT is the share's
    # dir inside it, and the SANDBOX path is a link at the device
    # mount's slot — the payload's contract is the sandbox layout,
    # the tmpfs placement is this init's.
    # The trace of last resort (live debugging, bd myconfig-2n8): the
    # guest console carries every step when the wrapper or the user
    # sets MYSBX_KRUN_TRACE=1 — a silent hang otherwise leaves
    # nothing to diagnose. Cost when off: one [ -n ] test per step.
    trace="''${MYSBX_KRUN_TRACE:-}"
    step() {
        [ -n "$trace" ] || return 0
        "$BB" echo "mysbx-init: $*"
    }

    shares="''${MYSBX_KRUN_SHARES:-}"
    while [ -n "$shares" ]; do
        entry=''${shares%%';'*}
        [ "$shares" = "$entry" ] && shares= || shares=''${shares#*';'}
        [ -n "$entry" ] || continue
        # One "DEVICE SLOT DEST ro|rw" entry per share (bd
        # myconfig-xpq): the DEVICE is the virtiofs tag (one per
        # access mode — the staged trees — plus the store's own),
        # mounted ONCE below; the SLOT is the share's dir inside it,
        # the DEST the payload's sandbox path.
        tag=''${entry%%' '*}
        rest=''${entry#*' '}
        slot=''${rest%%' '*}
        rest=''${rest#*' '}
        sandbox=''${rest%%' '*}
        mode=''${rest#*' '}
        mountpoint=/tmp/mysbx-shares/$tag
        step "placing share $sandbox (device $tag, slot $slot, $mode)"
        case " $mounted_devices " in
            *" $tag "*) ;;
            *)
                step "mounting device $tag ($mode) at $mountpoint"
                "$BB" mkdir -p "$mountpoint" \
                    || fail "cannot create the mountpoint $mountpoint"
                if [ "$mode" = ro ]; then
                    "$BB" mount -t virtiofs -o ro "$tag" "$mountpoint" \
                        || fail "cannot mount the virtiofs tag $tag at $mountpoint (ro)"
                else
                    "$BB" mount -t virtiofs "$tag" "$mountpoint" \
                        || fail "cannot mount the virtiofs tag $tag at $mountpoint (rw)"
                fi
                mounted_devices=" $mounted_devices $tag "
                ;;
        esac
        sharetarget=$mountpoint/$slot
        # The sandbox path already links at the mount (the rootfs
        # baked /nix/store and /mysbx-home): nothing to place.
        [ "$(  "$BB" readlink "$sandbox" || true)" = "$sharetarget" ] && continue
        # Where does the link live? /tmp/... for tmpfs-rooted paths,
        # /mysbx-home/... for the home's state shares (through the
        # baked /mysbx-home link), a fresh tmpfs at the FIRST
        # component for everything else (the ro root can hold no new
        # entries; mounting a tmpfs OVER a root-share subdirectory is
        # fine — libkrun's own implicit init mounts devtmpfs/proc/
        # sysfs exactly that way, the EBUSY of the spike's finding 9
        # is virtiofs-on-virtiofs only). The parents of the link are
        case "$sandbox" in
            /tmp/*|/mysbx-home/*)
                linktarget=$sandbox
                ;;
            /*/*)
                first=''${sandbox#/}
                first=/''${first%%/*}
                case " $tmpfs_first_components " in
                    *" $first "*) ;;
                    *)
                        "$BB" mount -t tmpfs tmpfs "$first" \
                            || fail "cannot mount the tmpfs for $sandbox"
                        tmpfs_first_components=" $tmpfs_first_components $first "
                        ;;
                esac
                # The parents of the link live ON the tmpfs just
                # mounted at $first — creating the real parent path
                # after the mount writes into the tmpfs.
                "$BB" mkdir -p "$(  "$BB" dirname "$sandbox")" \
                    || fail "cannot create the parents of $sandbox"
                linktarget=$sandbox
                ;;
            *)
                fail "the share path $sandbox cannot be placed (a krun mount dest needs a parent)"
                ;;
        esac
        # A state share may collide with an XDG dir the home tree
        # just created (.cache and friends): the share REPLACES it,
        # like the bwrap backend's later bind replaces the tmpfs dir.
        [ -e "$linktarget" ] && "$BB" rm -rf "$linktarget"
        step "linked $sandbox at $sharetarget"
        "$BB" ln -sfn "$sharetarget" "$linktarget" \
            || fail "cannot link $sandbox at $sharetarget"
    done

    # The payload is our own argv: $1 the path (absolute, no PATH
    # lookup needed), $2.. its arguments, exactly as krun_set_exec
    # carried them. krun_set_workdir already placed us in the
    # workspace (the spec's --chdir).
    [ "$#" -gt 0 ] || fail "no payload in argv — the krun_set_exec args did not reach the guest init"

    step "exec-ing the payload: $1"
    exec "$@"
  '';
in
runCommand "mysbx-krun-rootfs"
  {
    passthru.mysbxInit = mysbxInit;
    # The init text as a derivation attr, never interpolated into
    # the builder text: the script carries double quotes and
    # $(...), so bash would re-parse it. The env-var route hands the
    # bytes through verbatim (the spike rootfs's own pattern).
    initText = mysbxInit;
  }
  ''
    mkdir -p $out/bin $out/dev $out/proc $out/sys $out/tmp $out/nix
    # The share-root mountpoints (the crate's BAKED_SHARE_ROOTS —
    # keep the two lists identical): the guest init places a share
    # below one of these by mounting a tmpfs OVER the component,
    # which needs the mountpoint on the ro root; the builder refuses
    # a dest below anything else (never a run-time ENOENT).
    mkdir -p $out/etc $out/home $out/srv $out/mnt $out/media $out/opt $out/data
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
    # The payload's shells: store symlinks, valid once mysbx-init has
    # mounted the store share.
    ln -s ${bash}/bin/bash $out/bin/bash
    ln -s ${bash}/bin/sh $out/bin/sh
    printf '%s' "$initText" > $out/bin/mysbx-init
    chmod +x $out/bin/mysbx-init
  ''
