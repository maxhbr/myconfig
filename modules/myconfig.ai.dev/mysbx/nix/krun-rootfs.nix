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
  # The STATIC mkfs.ext4 of the scratch half (bd myconfig-dak.7,
  # backends.md D7): the init formats the per-run virtio-blk device
  # BEFORE any store path is visible — pkgsStatic, like busybox.
  e2fsprogsStatic,
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
  #
  # THE MANIFEST (the seventh live finding, the cmdline budget): the
  # x86 guest kernel copies only 2048 cmdline bytes
  # (COMMAND_LINE_SIZE), and a real config's env+shares block
  # measured 3075 — the tail silently never booted. The launcher
  # therefore writes env/shares/chdir into a MANIFEST FILE inside
  # the ro stage device and the cmdline carries only the pointer
  # (MYSBX_KRUN_MANIFEST=stage-ro:manifest). This init mounts the
  # stage-ro device, reads the file (tab-separated records, base64
  # values via the busybox applet) and only THEN applies them. The
  # legacy MYSBX_KRUN_SHARES env route remains for cmdline-sized
  # debug runs.
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

    # The trace of last resort (live debugging, bd myconfig-2n8):
    # the guest console carries every step when the wrapper or the
    # user sets MYSBX_KRUN_TRACE=1 — a silent hang otherwise leaves
    # nothing to diagnose. Defined BEFORE the first step call: sh
    # runs top to bottom, a call above the definition would try an
    # external `step` and die under set -eu before the first mount
    # (the fourth live finding). Cost when off: one [ -n ] test.
    trace="''${MYSBX_KRUN_TRACE:-}"
    step() {
        [ -n "$trace" ] || return 0
        "$BB" echo "mysbx-init: $*"
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
    manifest_chdir=""

    # The MANIFEST route (the cmdline budget finding, bd
    # myconfig-2n8's seventh): the launcher wrote env/share/chdir
    # records into the ro stage device; the cmdline carries only the
    # pointer. Mount stage-ro NOW — every share's link target
    # resolves through it — read the file, and apply the env lines
    # at once (the init's OWN steps use none of them; the payload
    # inherits everything through exec).
    manifest="''${MYSBX_KRUN_MANIFEST:-}"
    if [ -n "$manifest" ]; then
        mtag=''${manifest%%':'*}
        mslot=''${manifest#*':'}
        step "reading the manifest (device $mtag, slot $mslot)"
        mpoint=/tmp/mysbx-shares/$mtag
        "$BB" mkdir -p "$mpoint" \
            || fail "cannot create the manifest mountpoint $mpoint"
        "$BB" mount -t virtiofs -o ro "$mtag" "$mpoint" \
            || fail "cannot mount the manifest device $mtag"
        mounted_devices=" $mounted_devices $mtag "
        mpath=$mpoint/$mslot
        [ -f "$mpath" ] || fail "the manifest $mpath does not exist"
        # One record per line, tab-separated fields, base64 values:
        # a base64 value never contains a tab, the field split is
        # unambiguous.
        while IFS="	" read -r rkind r2 r3 r4 r5; do
            [ -n "$rkind" ] || continue
            case "$rkind" in
                env)
                    # r2 KEY, r3 base64(value)
                    val=$(printf '%s' "$r3" | "$BB" base64 -d) \
                        || fail "cannot decode the manifest env $r2"
                    export "$r2=$val"
                    ;;
                share)
                    # r2 DEVICE, r3 SLOT, r4 DEST, r5 ro|rw
                    [ -n "$r5" ] || fail "manifest share record $r2:$r3 is short a mode"
                    share_records="''${share_records:-}
    $r2 $r3 $r4 $r5"
                    ;;
                chdir)
                    # r2 base64(dir) — applied AFTER the shares exist
                    manifest_chdir=$(printf '%s' "$r2" | "$BB" base64 -d) \
                        || fail "cannot decode the manifest chdir"
                    ;;
                *)
                    fail "unknown manifest record: $rkind"
                    ;;
            esac
        done < "$mpath"
    fi

    # Each share: one "DEVICE SLOT SANDBOX_PATH ro|rw" record — the
    # manifest's share lines above, or the legacy
    # MYSBX_KRUN_SHARES env (';'-separated, cmdline-sized debug
    # runs only — the seventh live finding killed that route for
    # real configs). Each device is mounted ONCE under
    # /tmp/mysbx-shares keyed by its tag; the SLOT is the share's
    # dir inside it, and the SANDBOX path is a link at the device
    # mount's slot — the payload's contract is the sandbox layout,
    # the tmpfs placement is this init's.
    share_records="''${share_records:-}"
    if [ -z "$manifest" ]; then
        shares="''${MYSBX_KRUN_SHARES:-}"
        while [ -n "$shares" ]; do
            entry=''${shares%%';'*}
            [ "$shares" = "$entry" ] && shares= || shares=''${shares#*';'}
            [ -n "$entry" ] || continue
            share_records="$share_records
    $entry"
        done
    fi
    # The records as a FILE, read with input redirection (NOT a
    # pipeline: the `while` of a pipeline runs in a SUBSHELL, and a
    # mount done there must not be relied on — the live sim showed
    # the device mount vanishing with the subshell on one host).
    # Redirection keeps the loop — and every mount it performs — in
    # THIS shell.
    records_file=/tmp/mysbx-share-records
    printf '%s\n' "$share_records" > "$records_file"
    while IFS=" " read -r tag slot sandbox mode; do
        [ -n "$tag" ] || continue
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
                # The parents on the tmpfs (the ninth live finding:
                # the home tree creates only the XDG roots — a share
                # below a DEEPER path, .config/workmux/config.yaml,
                # needs its parents created, `ln` does not).
                "$BB" mkdir -p "$(  "$BB" dirname "$sandbox")" \
                    || fail "cannot create the parents of $sandbox"
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
    done < "$records_file"
    "$BB" rm -f "$records_file"

    # The guest resolver (bd myconfig-dak.6, backends.md D6): TSI
    # dials from the launcher's netns — the HOST netns — so the
    # host's resolver addresses are exactly what the proxy can
    # reach. Nothing else writes /etc/resolv.conf inside the guest
    # (TSI has no net device, no DHCP); the manifest's env is the
    # transport. The share loop's tmpfs-at-/etc (when it ran) is
    # already in place; on a rootfs without /etc shares this mounts
    # the component first. After the resolver exists, nothing must
    # mount over /etc.
    if [ -n "''${MYSBX_KRUN_RESOLV:-}" ]; then
        step "writing /etc/resolv.conf from the host resolver"
        # /etc is the ro root's share-root unless a share below it
        # already tmpfs'd it: probe writability, mount when ro.
        if ! "$BB" touch /etc/.mysbx-resolv-probe 2>/dev/null; then
            "$BB" mount -t tmpfs tmpfs /etc \
                || fail "cannot mount the tmpfs for /etc (the resolver)"
        else
            "$BB" rm -f /etc/.mysbx-resolv-probe
        fi
        # The env record was ALREADY decoded by the loader (every
        # manifest env line exports plain text) — write verbatim.
        printf '%s\n' "$MYSBX_KRUN_RESOLV" > /etc/resolv.conf \
            || fail "cannot write /etc/resolv.conf"
    fi

    # The scratch disk (bd myconfig-dak.7, backends.md D7): a REAL
    # virtio-blk device the launcher attached (krun_add_disk2), the
    # per-run raw file — no stale fs ever survives to be probed.
    # Identified by PRESENCE, never by name: the block_id names the
    # MMIO slot host-side, it is no serial the guest reads; the
    # scratch is the ONLY /dev/vd*. The manifest env announces it
    # (MYSBX_KRUN_SCRATCH=1); a run without it keeps the plain ro
    # store share — no silent RAM fallback.
    #
    # The disk becomes the nix scratch: the ext4 carries the overlay
    # upper/work over the ro store share, nix state, logs, cache and
    # TMPDIR — the guest kernel's OWN filesystem (chown and overlay
    # xattrs work natively, nothing over the virtiofs xattr
    # surface). The /nix placement is DELIBERATE (bd
    # myconfig-anw): the overlay mounts at /nix/store itself, ON
    # TOP of the store share's mount — no generic tmpfs ever mounts
    # at /nix, the baked link stays the lower layer's path.
    if [ "''${MYSBX_KRUN_SCRATCH:-}" = "1" ]; then
        # The device: the only /dev/vd* (the root is virtiofs, not
        # blk; no other disk ever attaches).
        dev=$(  "$BB" ls /dev/vd* 2>/dev/null) \
            || fail "MYSBX_KRUN_SCRATCH=1 but no /dev/vd* device — the disk did not attach"
        [ "$(  printf '%s\n' "$dev" | "$BB" wc -l)" -eq 1 ] \
            || fail "MYSBX_KRUN_SCRATCH=1 but more than one /dev/vd* device: $dev"
        step "formatting $dev as the nix scratch (ext4)"
        /bin/mkfs.ext4 -q -F "$dev" \
            || fail "cannot mkfs.ext4 the scratch device $dev"
        # The mountpoints: the scratch root and the overlay's work
        # parent live ON the disk; the overlay target is /nix/store
        # itself (the baked link's dest — mounting over a symlink
        # fails, so bind it to itself first, a no-op mountpoint
        # maker).
        "$BB" mkdir -p /mysbx-nix
        "$BB" mount -t ext4 "$dev" /mysbx-nix \
            || fail "cannot mount the scratch $dev at /mysbx-nix"
        "$BB" mkdir -p /mysbx-nix/store-upper /mysbx-nix/store-work \
            || fail "cannot create the overlay dirs on the scratch"
        # /nix/store is a baked SYMLINK into the stage tree, and a
        # mount THROUGH a symlink lands at the link's TARGET — on
        # the share mount that shadows the whole stage device's
        # tree. The overlay needs a REAL mountpoint at the link's
        # PATH. The tmpfs at /nix SHADOWS the baked link — nothing
        # of the ro root is visible under it, no removal needed;
        # the real dir lives on the tmpfs, then the overlay mounts
        # at the link's own path. The lowerdir names the SHARE's
        # backing path directly.
        "$BB" mount -t tmpfs tmpfs /nix \
            || fail "cannot mount the tmpfs for /nix (the overlay's mountpoint root)"
        "$BB" mkdir -p /nix/store \
            || fail "cannot create the real /nix/store mountpoint"
        "$BB" mount -t overlay overlay \
            -o lowerdir=/tmp/mysbx-shares/stage-ro/store,upperdir=/mysbx-nix/store-upper,workdir=/mysbx-nix/store-work \
            /nix/store \
            || fail "cannot mount the store overlay (scratch upper)"
        step "the nix scratch is $dev at /mysbx-nix; the store overlay is up"
    fi

    # The payload is our own argv: $1 the path (absolute, no PATH
    # lookup needed), $2.. its arguments, exactly as krun_set_exec
    # carried them.
    [ "$#" -gt 0 ] || fail "no payload in argv — the krun_set_exec args did not reach the guest init"

    # The workdir, applied AFTER the shares exist (the manifest's
    # chdir record — the cmdline route had /init.krun chdir BEFORE
    # the workspace share was mounted, silently landing at /).
    if [ -n "$manifest_chdir" ]; then
        step "chdir to the workspace: $manifest_chdir"
        cd "$manifest_chdir" \
            || fail "cannot chdir to the workdir $manifest_chdir"
    fi

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
    # The payload's shells: store symlinks, valid once mysbx-init has
    # mounted the store share.
    ln -s ${bash}/bin/bash $out/bin/bash
    ln -s ${bash}/bin/sh $out/bin/sh
    printf '%s' "$initText" > $out/bin/mysbx-init
    chmod +x $out/bin/mysbx-init
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
