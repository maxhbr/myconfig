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
  # escapeShellArg of the reconcile SQL below: the init text is a
  # Nix indented string whose '' rules make inlined SQL quoting
  # error-prone — the quoted SQL is generated here instead.
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
  # The db reconcile's SQL, shell-quoted by Nix (the init text
  # below interpolates it verbatim): the pattern needs SQL single
  # quotes, which the indented-string '' rules make painful to
  # inline — generating the quoting here keeps the init readable.
  # A store path's name carries no quote, so no escaping of VALUES
  # is needed (only the LIKE pattern here is a literal).
  reconcileSelect = lib.strings.escapeShellArg "SELECT path FROM ValidPaths WHERE path LIKE '/nix/store/%'";
  # The `#` of the step timer's `10#` base prefix: ash needs it
  # to parse a leading-zero uptime fraction as decimal, but inside
  # the indented init string a bare `#` would open a Nix comment
  # and a bare `${` would interpolate — interpolating this plus the
  # `''${` escape builds the shell bytes.
  basePrefix = "#";
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
  # (MYSBX_KRUN_MANIFEST=stage-ro:manifest). The qemu bridge adds
  # one record kind: the payload argv (bd myconfig-hq7k) — the stock
  # kernel's `--` cmdline split is lossy (whitespace-split,
  # `=`-tokens to init's envp, MAX_INIT_ARGS), so the qemu launcher
  # carries the payload ONLY here. The krun launcher writes no argv
  # records (libkrun's envp route is lossless). This init mounts
  # the stage-ro device, reads the file (tab-separated records,
  # base64 values via the busybox applet) and only THEN applies
  # them. The legacy MYSBX_KRUN_SHARES env route remains for
  # cmdline-sized debug runs.
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

    # PID-1 boot (the qemu bridge, backends.md D9): the krun guest
    # runs after libkrun's implicit init mounted proc/sys/dev +
    # devpts + /dev/shm + the /dev/fd link; the qemu guest runs this
    # script AS PID 1 (the kernel's init=), so the mounts the init
    # needs are not up yet — except /dev: the pinned nixpkgs kernel
    # has CONFIG_DEVTMPFS_MOUNT=y and automounts devtmpfs on /dev
    # right after mounting root, so the init's own devtmpfs mount
    # would hit EBUSY. A mounted devtmpfs IS the goal — the
    # /proc/mounts probe accepts it and moves on. The /proc/uptime
    # probe keeps the krun path unchanged:
    # mounted pseudo-filesystems skip every mount. The devpts mount
    # is what makes PTYs exist at all (tmux/mux sessions allocate
    # them in the guest — the supported session path); /dev/shm is
    # POSIX shared memory; /dev/fd is the standard fd redirection
    # link; lo up makes 127.0.0.1 bindable (the baked /etc/hosts
    # localhost mapping is dead weight without it) (bd
    # myconfig-af0j).
    if [ ! -f /proc/uptime ]; then
        "$BB" mount -t proc proc /proc \
            || fail "cannot mount proc as pid 1"
        "$BB" mount -t sysfs sysfs /sys \
            || fail "cannot mount sysfs as pid 1"
        "$BB" grep -q ' /dev devtmpfs ' /proc/mounts \
            || "$BB" mount -t devtmpfs devtmpfs /dev \
            || fail "cannot mount devtmpfs as pid 1"
        "$BB" mkdir -p /dev/pts /dev/shm
        "$BB" mount -t devpts devpts /dev/pts \
            || fail "cannot mount devpts as pid 1"
        "$BB" mount -t tmpfs tmpfs /dev/shm \
            || fail "cannot mount /dev/shm as pid 1"
        "$BB" ln -s /proc/self/fd /dev/fd \
            || fail "cannot link /dev/fd as pid 1"
        "$BB" ip link set lo up \
            || fail "cannot bring up lo as pid 1"
    fi

    # The mysbx.* cmdline params (the qemu bridge): libkrun hands the
    # guest its settings through envp; qemu has no envp — the kernel
    # cmdline carries them instead (mysbx.trace=, mysbx.manifest=,
    # mysbx.exit-channel=). Env stays first-class: every consumer
    # below falls back to _cval ONLY when its variable is unset, and
    # the krun cmdline (libkrun's own) carries no mysbx.* params.
    _cval=""
    _cmdline_mysbx() {
        # $1: the param name; sets _cval to the LAST value before
        # the `--` split (empty when absent). The payload argv
        # follows `--` and must not be parsed as a param.
        _cval=""
        for _cw in $("$BB" cat /proc/cmdline 2>/dev/null); do
            case "$_cw" in
                --) break ;;
                "mysbx.$1="*) _cval="''${_cw#*=}" ;;
            esac
        done
    }

    # The trace of last resort (live debugging, bd myconfig-2n8):
    # the guest console carries every step when the wrapper or the
    # user sets MYSBX_KRUN_TRACE=1 — a silent hang otherwise leaves
    # nothing to diagnose. Defined BEFORE the first step call: sh
    # runs top to bottom, a call above the definition would try an
    # external `step` and die under set -eu before the first mount
    # (the fourth live finding). Cost when off: one [ -n ] test.
    # The launcher's --verbose arms the trace too, so a verbose
    # launch narrates the boot gap: the launcher's own phase lines
    # stop at "vm setup" and resume only after guest exit — the
    # steps between are exactly what the init spends the gap on.
    # Each traced step carries the kernel uptime in [brackets] and
    # its own duration since the previous step (the first one
    # therefore measures from kernel boot — it lands late when the
    # kernel spent long booting before the init started).
    # busybox date has no %N (no sub-second clock), but
    # /proc/uptime carries the fraction; the 10# prefixes keep a
    # leading-zero fraction like ".08" from tripping ash's octal
    # parsing. A read failure (no /proc yet) reports 0 — timing is
    # best-effort, the step names are the point.
    trace="''${MYSBX_KRUN_TRACE:-}"
    if [ -z "$trace" ]; then
        _cmdline_mysbx trace
        if [ "$_cval" = "1" ]; then trace=1; fi
    fi
    _now_ms() {
        if read _up _ < /proc/uptime 2>/dev/null; then
            _ms=$(( (10${basePrefix}''${_up%%.*} * 100 + 10${basePrefix}''${_up#*.}) * 10 ))
        else
            _ms=0
        fi
    }
    step() {
        [ -n "$trace" ] || return 0
        _now_ms
        "$BB" echo "mysbx-init: [''${_ms}ms] $* (+$((_ms - _step_t0)) ms)"
        _step_t0=$_ms
    }
    _step_t0=0

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
    # and the share's mountpoint then lives on writable tmpfs. Mounted
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
    if [ -z "$manifest" ]; then
        _cmdline_mysbx manifest
        manifest=$_cval
    fi
    # The exit channel (the qemu bridge, backends.md D9): the
    # virtio-serial port the payload's exit code travels back on.
    # Cmdline-only — the krun guest has no channel and never sets
    # one.
    exit_channel=""
    _cmdline_mysbx exit-channel
    exit_channel=$_cval
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
        # The payload argv records (bd myconfig-hq7k, the qemu
        # bridge): the stock kernel's `--` cmdline split is LOSSY
        # (whitespace-split, `=`-tokens to init's envp,
        # MAX_INIT_ARGS), so a manifest-bearing run carries the
        # payload here instead — one `argv <index> <base64>` record
        # per token, indices ascending. Each index is validated
        # against the running count (a truncated or reordered
        # manifest is a diagnosis, never a mangled payload); the
        # decoded tokens REPLACE this init's own $* on the first
        # record and append after (set -- keeps each token whole —
        # no re-splitting, an EMPTY token survives). The krun
        # guest rides envp, never sets a manifest, and its $* is
        # untouched.
        _argv_n=0
        # One record per line, tab-separated fields, base64 values:
        # a base64 value never contains a tab, the field split is
        # unambiguous.
        while IFS="	" read -r rkind r2 r3 r4 r5; do
            [ -n "$rkind" ] || continue
            case "$rkind" in
                env)
                    # r2 KEY, r3 base64(value). The sentinel x:
                    # $( ) strips ALL trailing newlines from the
                    # substitution (POSIX), so a value ENDING in a
                    # newline would decode short (bd
                    # myconfig-m6r1) — the sentinel makes the strip
                    # exact: only the x itself goes (the && keeps
                    # the || fail refusal semantics on a decode
                    # error).
                    val=$(printf '%s' "$r3" | "$BB" base64 -d && printf x) \
                        || fail "cannot decode the manifest env $r2"
                    val=''${val%x}
                    export "$r2=$val"
                    ;;
                share)
                    # r2 DEVICE, r3 SLOT, r4 DEST, r5 ro|rw
                    [ -n "$r5" ] || fail "manifest share record $r2:$r3 is short a mode"
                    share_records="''${share_records:-}
    $r2 $r3 $r4 $r5"
                    ;;
                argv)
                    # r2 index, r3 base64(token) — one record per
                    # payload token, the index validated against
                    # the running count (bd myconfig-hq7k). The
                    # first record DISCARDS the cmdline-passed $*
                    # (a manifest-bearing qemu run never puts the
                    # payload on the cmdline; anything the kernel
                    # split behind `--` is not the payload), the
                    # rest append (set -- keeps each token whole —
                    # no re-splitting, an EMPTY token survives
                    # losslessly).
                    [ "$r2" -eq "$_argv_n" ] \
                        || fail "manifest argv index $r2 is out of order (expected $_argv_n)"
                    # The same sentinel as the env record (bd
                    # myconfig-m6r1): a token ENDING in a newline
                    # must arrive byte-identical — $( ) alone
                    # strips it.
                    val=$(printf '%s' "$r3" | "$BB" base64 -d && printf x) \
                        || fail "cannot decode the manifest argv $_argv_n"
                    val=''${val%x}
                    if [ "$_argv_n" -eq 0 ]; then
                        set --
                    fi
                    set -- "$@" "$val"
                    _argv_n=$((_argv_n + 1))
                    ;;
                chdir)
                    # r2 base64(dir) — applied AFTER the shares exist
                    # (the same sentinel decode, bd myconfig-m6r1)
                    manifest_chdir=$(printf '%s' "$r2" | "$BB" base64 -d && printf x) \
                        || fail "cannot decode the manifest chdir"
                    manifest_chdir=''${manifest_chdir%x}
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
    # dir inside it, and the SANDBOX path is a bind mount of the
    # device mount's slot — the payload's contract is the sandbox
    # layout, the tmpfs placement is this init's.
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
        # The LINKLESS shares (bd myconfig-j23): consumed directly at
        # their backing path, never linked into the sandbox layout.
        [ "$sandbox" = "-" ] && continue
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
        # Where does the mountpoint live? /tmp/... for tmpfs-rooted
        # paths, /mysbx-home/... for the home's state shares (through
        # the baked /mysbx-home link), a fresh tmpfs at the FIRST
        # component for everything else (the ro root can hold no new
        # entries; mounting a tmpfs OVER a root-share subdirectory is
        # fine — libkrun's own implicit init mounts devtmpfs/proc/
        # sysfs exactly that way, the EBUSY of the spike's finding 9
        # is virtiofs-on-virtiofs only).
        case "$sandbox" in
            /tmp/*|/mysbx-home/*)
                # The parents on the tmpfs (the ninth live finding:
                # the home tree creates only the XDG roots — a share
                # below a DEEPER path, .config/workmux/config.yaml,
                # needs its parents created).
                "$BB" mkdir -p "$(  "$BB" dirname "$sandbox")" \
                    || fail "cannot create the parents of $sandbox"
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
                # The parents live ON the tmpfs just mounted at
                # $first — creating the real parent path after the
                # mount writes into the tmpfs.
                "$BB" mkdir -p "$(  "$BB" dirname "$sandbox")" \
                    || fail "cannot create the parents of $sandbox"
                ;;
            *)
                fail "the share path $sandbox cannot be placed (a krun mount dest needs a parent)"
                ;;
        esac
        # The sandbox path is a BIND MOUNT of the device slot, not a
        # symlink: getcwd() and realpath() then report the sandbox
        # path, as on the bwrap/podman backends, so tools that
        # canonicalize (git worktree metadata, aoe, herdr) never see
        # the /tmp/mysbx-shares spelling. A bind is no new virtiofs
        # device, so finding 9 does not apply. The mountpoint takes
        # the slot's type; a state share mounts OVER an XDG dir the
        # home tree created (.cache and friends), like the bwrap
        # backend's later bind over the tmpfs dir. Nothing is ever
        # removed recursively: only a symlink or a type-mismatched
        # empty entry is replaced.
        if [ -d "$sharetarget" ]; then
            if [ -L "$sandbox" ] || { [ -e "$sandbox" ] && [ ! -d "$sandbox" ]; }; then
                "$BB" rm -f "$sandbox" \
                    || fail "cannot replace $sandbox with a directory mountpoint"
            fi
            "$BB" mkdir -p "$sandbox" \
                || fail "cannot create the mountpoint $sandbox"
        else
            if [ -L "$sandbox" ]; then
                "$BB" rm -f "$sandbox" \
                    || fail "cannot replace $sandbox with a file mountpoint"
            elif [ -d "$sandbox" ]; then
                "$BB" rmdir "$sandbox" \
                    || fail "cannot replace the directory $sandbox with a file mountpoint"
            fi
            [ -e "$sandbox" ] || : > "$sandbox" \
                || fail "cannot create the mountpoint $sandbox"
        fi
        # The canonical mountpoint: a parent may be a symlink (the
        # baked /mysbx-home), and busybox's remount looks the target
        # up in /proc/mounts by its literal, resolved spelling.
        mnt=$("$BB" readlink -f "$sandbox") \
            || fail "cannot resolve the mountpoint $sandbox"
        step "binding $sandbox ($mnt) to $sharetarget"
        "$BB" mount -o bind "$sharetarget" "$mnt" \
            || fail "cannot bind $sharetarget at $sandbox"
        if [ "$mode" = ro ]; then
            "$BB" mount -o remount,bind,ro "$mnt" \
                || fail "cannot make the bind of $sandbox read-only"
        fi
    done < "$records_file"
    "$BB" rm -f "$records_file"

    # The guest resolver (bd myconfig-dak.6, backends.md D6): TSI
    # dials from the launcher's netns — the HOST netns — so the
    # host's resolver addresses are exactly what the proxy can
    # reach. Nothing else writes /etc/resolv.conf inside the guest
    # (TSI has no net device, no DHCP); the manifest's env is the
    # transport. The share loop's tmpfs-at-/etc (when it ran) is
    # already in place; on a rootfs without /etc shares this mounts
    # the component first. After the resolver exists, only the
    # SLiRP step below may write it (the DNS rewrite, bd
    # myconfig-m0to).
    if [ -n "''${MYSBX_KRUN_RESOLV:-}" ]; then
        step "writing /etc/resolv.conf from the host resolver"
        # /etc is the ro root's share-root unless a share below it
        # already tmpfs'd it: probe writability, mount when ro.
        if ! "$BB" touch /etc/.mysbx-resolv-probe 2>/dev/null; then
            # Copy the baked /etc/hosts before the tmpfs hides the
            # ro root's /etc: the guest has no other source of the
            # localhost → 127.0.0.1 mapping, and without it Node.js
            # fetch() fails with getaddrinfo ENOTFOUND localhost
            # (curl resolves localhost itself, so it masked the gap).
            hosts_backup=/tmp/.mysbx-etc-hosts
            "$BB" cp /etc/hosts "$hosts_backup" 2>/dev/null || rm -f "$hosts_backup"
            "$BB" mount -t tmpfs tmpfs /etc \
                || fail "cannot mount the tmpfs for /etc (the resolver)"
            [ -f "$hosts_backup" ] && "$BB" cp "$hosts_backup" /etc/hosts || true
            "$BB" rm -f "$hosts_backup"
        else
            "$BB" rm -f /etc/.mysbx-resolv-probe
        fi
        # Ensure /etc/hosts exists on the (possibly tmpfs-backed)
        # /etc: the share loop may have mounted a tmpfs over /etc
        # before this code ran, hiding the baked file. The guest
        # has no other source of the localhost → 127.0.0.1 mapping.
        if [ ! -f /etc/hosts ]; then
            printf '127.0.0.1 localhost\n::1 localhost\n' > /etc/hosts \
                || fail "cannot write /etc/hosts"
        fi
        # The env record was ALREADY decoded by the loader (every
        # manifest env line exports plain text) — write verbatim.
        printf '%s\n' "$MYSBX_KRUN_RESOLV" > /etc/resolv.conf \
            || fail "cannot write /etc/resolv.conf"
    else
        # No resolver to write, but /etc/hosts may still be missing
        # if the share loop mounted a tmpfs over /etc.
        if [ ! -f /etc/hosts ]; then
            printf '127.0.0.1 localhost\n::1 localhost\n' > /etc/hosts 2>/dev/null || true
        fi
    fi

    # The SLiRP NIC of the qemu backend (bd myconfig-k106, the qemu
    # bridge): libkrun's network is TSI (the guest has NO ethernet
    # device — the resolver step above is the whole story there),
    # but qemu's `-netdev user` attaches a REAL virtio-net-pci NIC
    # that nothing configures guest-side: without an address and a
    # route, resolv.conf alone resolves nothing. The marker env is
    # set ONLY by the microvm-qemu builder when the network is
    # shared (the krun backend sets none — no step runs, no eth0
    # exists to configure). qemu's user netdev carries FIXED SLiRP
    # defaults — guest 10.0.2.15/24, gateway 10.0.2.2, DNS proxy
    # 10.0.2.3 — so the config is static, no DHCP. The interface
    # is found by DRIVER (the first virtio_net device), never by a
    # name the kernel's probe order could change. The host's own
    # resolver addresses are host-local (e.g. systemd-resolved's
    # 127.0.0.53): behind the SLiRP NAT they point into the GUEST,
    # where nothing listens — DNS would be dead (bd
    # myconfig-m0to). SLiRP's DNS proxy is the .3 address, NOT the
    # .2 gateway; the resolver written above is therefore REWRITTEN
    # to the proxy (the krun arm never runs this — TSI dials from
    # the HOST netns, where the host's addresses are exactly right).
    if [ "''${MYSBX_MICROVM_NET:-}" = "1" ]; then
        _nic=""
        for _if in /sys/class/net/*; do
            # -e, never -f: a sysfs driver bind is a symlink to a
            # DIRECTORY — [ -f ] follows the link and is false for
            # it (the live-shaped fixture caught this).
            [ -e "$_if/device/driver" ] || continue
            case "$(  "$BB" readlink "$_if/device/driver")" in
                *virtio_net) _nic="$(  "$BB" basename "$_if")" ;;
            esac
            [ -n "$_nic" ] && break
        done
        if [ -n "$_nic" ]; then
            step "configuring the SLiRP NIC $_nic (10.0.2.15/24 via 10.0.2.2)"
            "$BB" ip addr add 10.0.2.15/24 dev "$_nic" \
                || fail "cannot address the NIC $_nic"
            "$BB" ip link set "$_nic" up \
                || fail "cannot bring the NIC $_nic up"
            "$BB" ip route add default via 10.0.2.2 \
                || fail "cannot add the default route via 10.0.2.2"
            # The SLiRP DNS proxy — the host's resolver addresses
            # are unreachable from behind the NAT (bd
            # myconfig-m0to). A fresh tmpfs (no /etc shares) keeps
            # the baked /etc/hosts; a tmpfs'd /etc carries it —
            # the localhost mapping must survive the rewrite.
            if ! "$BB" touch /etc/.mysbx-resolv-probe 2>/dev/null; then
                "$BB" mount -t tmpfs tmpfs /etc \
                    || fail "cannot mount the tmpfs for /etc (the SLiRP resolver)"
            else
                "$BB" rm -f /etc/.mysbx-resolv-probe
            fi
            [ -f /etc/hosts ] || "$BB" printf '%s\n' \
                '127.0.0.1 localhost' > /etc/hosts
            "$BB" printf 'nameserver 10.0.2.3\n' > /etc/resolv.conf \
                || fail "cannot write the SLiRP resolver"
        else
            fail "MYSBX_MICROVM_NET=1 but no virtio_net interface exists — the NIC did not attach"
        fi
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
    # surface). The overlay mounts at /nix/store itself. Until db
    # validation finishes, the baked /nix/store link exposes the
    # bare read-only share; a tmpfs then supplies a real mountpoint.
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
        # The copied database and the overlay's upper/work live on
        # the freshly formatted disk.
        "$BB" mkdir -p /mysbx-nix
        "$BB" mount -t ext4 "$dev" /mysbx-nix \
            || fail "cannot mount the scratch $dev at /mysbx-nix"

        # The guest nix state (bd myconfig-j23): the db copy the
        # podman image bakes via includeNixDB, done here because the
        # direct rootfs carries none. The LINKLESS nix-var share
        # (stage-ro, the network gate's twin of the bwrap tier's
        # /nix/var/nix bind) holds the HOST's db; copying it
        # registers every shared store path as valid, so the guest's
        # single-user nix never re-adds — the live fchmodat2 crash
        # was exactly a re-add deleting a lower-layer entry through
        # the overlay. ONLY the db: the image's var copy may take the
        # whole tree, but a HOST's live var also carries builds/
        # (0700 per-build log dirs owned by root/nixbld, unreadable
        # through the share) and profiles/gcroots/temproots session
        # state the guest never needs — the single-user nix creates
        # its own. big-lock/reserved are 0600 root-owned and carry
        # no data (a lock and reserved space); nix recreates both.
        if [ -d /tmp/mysbx-shares/stage-ro/nix-var/db ]; then
            step "copying the host nix db to the scratch"
            "$BB" mkdir -p /mysbx-nix/state/db \
                || fail "cannot create the nix state dir on the scratch"
            for f in /tmp/mysbx-shares/stage-ro/nix-var/db/*; do
                [ -e "$f" ] || continue
                case "''${f##*/}" in
                    big-lock | reserved) continue ;;
                esac
                "$BB" cp -R "$f" /mysbx-nix/state/db/ \
                    || fail "cannot copy the nix db entry ''${f##*/}"
            done
        else
            step "no nix-var db — the guest nix db starts empty"
        fi
        # The db reconcile (bd myconfig-mxu, the direct twin of the
        # podman setup wrapper's step): the copied db is a SNAPSHOT
        # of host state, the staged store share is LIVE — a host
        # rebuild restaging the share leaves rows for paths the
        # share no longer carries, and an in-session fetch once
        # registered a flake source as a store SYMLINK
        # (doc/TODOs/krun-guest-flake-source-symlink.md). Both
        # divergences break every later eval that trusts the row
        # ("is a symlink" / "Stale file handle") and nix's re-add
        # of the lower-layer entry crashes (fchmodat2). Prune the
        # rows the share cannot keep against the bare mounted ro
        # share, before overlay dentries can retain its inodes.
        # The freshly formatted scratch has no upper entries, so
        # the baked /nix/store link sees the same paths. POSIX sh +
        # the baked static sqlite3 (a real copy like busybox):
        # [ -e ] is FALSE for a dangling symlink, so both poison
        # shapes (dangling and live-target) prune, as does a plain
        # missing path. `nix store delete` cannot do this job
        # (liveness semantics, plus its store-tree unlink is
        # exactly the lower-layer crash).
        if [ -f /mysbx-nix/state/db/db.sqlite ]; then
            registered_file=/mysbx-nix/registered-paths.txt
            scan_file=/mysbx-nix/store-scan.txt
            recheck_file=/mysbx-nix/recheck-paths.txt
            doomed_file=/mysbx-nix/doomed-paths.txt
            step "enumerating the registered store paths from the copied nix db"
            /bin/sqlite3 /mysbx-nix/state/db/db.sqlite \
                ${reconcileSelect} \
                > "$registered_file" \
                || fail "cannot enumerate registered store paths in the copied nix database"
            n_registered=$("$BB" wc -l < "$registered_file")
            : > "$doomed_file"
            # One bulk getdents pass replaces the per-path stat: every
            # [ -e ]/[ -L ] of the old loop was one virtiofs FUSE round
            # trip, and ~175k registered paths made the reconcile the
            # minute of the boot (bd myconfig-kid). The scan carries
            # each entry's d_type straight from the readdir stream
            # (libkrun passes the host's through), the awk join
            # classifies all rows in bulk, and only the rows it
            # cannot classify — DT_UNKNOWN (d_type 0), a non-directory
            # share root entry, or a path deeper than the scanned root
            # — fall back to the old per-path stat loop, exact
            # semantics kept: [ -e ] false for missing AND dangling
            # symlinks, [ -L ] true for every symlink (live-target
            # poison included).
            step "scanning the read-only store share (bulk getdents)"
            /bin/store-scan /nix/store > "$scan_file" \
                || fail "cannot scan the read-only store share"
            step "checking $n_registered registered store path(s) against the read-only store share"
            "$BB" awk -v store_prefix="/nix/store/" -v doomed_file="$doomed_file" -v recheck_file="$recheck_file" '
                NR == FNR { entry[$2] = $1; next }
                {
                    if (index($0, store_prefix) != 1) next
                    p = substr($0, length(store_prefix) + 1)
                    deep = index(p, "/")
                    name = deep > 0 ? substr(p, 1, deep - 1) : p
                    if (name == "") next
                    t = entry[name]
                    if (t == "" || t == 10)
                        print $0 >> doomed_file
                    else if (deep > 0 || t == 0 || t == 4)
                        print $0 >> recheck_file
                }
            ' "$scan_file" "$registered_file" \
                || fail "cannot join the registered paths against the store scan"
            "$BB" rm -f "$scan_file" "$registered_file"
            # The fallback stat loop: only the rows the scan could not
            # classify (at most the DT_UNKNOWN and deep ones), the
            # same [ -e ]/[ -L ] semantics the old full loop had.
            if [ -s "$recheck_file" ]; then
                while IFS= read -r p; do
                    [ -n "$p" ] || continue
                    if [ ! -e "$p" ] || [ -L "$p" ]; then
                        printf '%s\n' "$p" >> "$doomed_file"
                    fi
                done < "$recheck_file"
            fi
            "$BB" rm -f "$recheck_file"
            n_doomed=$("$BB" wc -l < "$doomed_file")
            if [ "$n_doomed" -gt 0 ]; then
                step "reconciling the db: $n_doomed of $n_registered registered store path(s) missing or symlinked, dropping their rows"
                {
                    printf 'PRAGMA foreign_keys=ON;\nBEGIN;\n'
                    while IFS= read -r p; do
                        [ -n "$p" ] || continue
                        printf "DELETE FROM Refs WHERE reference = (SELECT id FROM ValidPaths WHERE path = '%s');\n" "$p"
                        printf "DELETE FROM ValidPaths WHERE path = '%s';\n" "$p"
                    done < "$doomed_file"
                    printf 'COMMIT;\n'
                } | /bin/sqlite3 -bail /mysbx-nix/state/db/db.sqlite \
                    || fail "cannot reconcile dependent rows in the copied nix database"
            fi
            "$BB" rm -f "$doomed_file"
            fk_violations=$(/bin/sqlite3 /mysbx-nix/state/db/db.sqlite 'PRAGMA foreign_key_check;') \
                || fail "cannot check foreign keys in the copied nix database"
            [ -z "$fk_violations" ] \
                || fail "foreign-key violations remain in the copied nix database"
            step "copied nix db validation complete"
        fi

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
        step "mounting the store overlay (fresh scratch upper)"
        "$BB" mount -t overlay overlay \
            -o lowerdir=/tmp/mysbx-shares/stage-ro/store,upperdir=/mysbx-nix/store-upper,workdir=/mysbx-nix/store-work \
            /nix/store \
            || fail "cannot mount the store overlay (scratch upper)"

        "$BB" mkdir -p /mysbx-nix/log /mysbx-nix/cache /mysbx-nix/tmp \
            || fail "cannot create the nix log/cache/tmp dirs on the scratch"
        # The single-user nix environment (the podman image's
        # wrapper env, the direct twin): NIX_REMOTE=local (no daemon
        # in the VM), state/log/cache/TMPDIR on the scratch. NIX_CONFIG
        # composes defaults FIRST so a config [nix] layer (the
        # manifest env) overrides them.
        export NIX_REMOTE=local
        export NIX_STATE_DIR=/mysbx-nix/state
        export NIX_LOG_DIR=/mysbx-nix/log
        export NIX_CACHE_HOME=/mysbx-nix/cache
        export TMPDIR=/mysbx-nix/tmp
        # One directive per line, built by append so no literal
        # newline ever sits at a column the indented-string
        # stripping would mangle (the shebang guard's own rule).
        nix_config_defaults="build-users-group ="
        nix_config_defaults="$nix_config_defaults
    sandbox = false"
        nix_config_defaults="$nix_config_defaults
    experimental-features = nix-command flakes"
        if [ -n "''${NIX_CONFIG:-}" ]; then
            nix_config_defaults="$nix_config_defaults
    $NIX_CONFIG"
        fi
        export NIX_CONFIG="$nix_config_defaults"
        step "the nix scratch is $dev at /mysbx-nix; the store overlay is up"
    fi

    # The payload is our own argv: the manifest's argv records
    # REPLACED $* when one exists (bd myconfig-hq7k); else the
    # kernel's `--` split handed the krun_set_exec vector over as
    # $1.. (the krun guest's own route — and the qemu debug run
    # without a manifest).
    [ "$#" -gt 0 ] || fail "no payload — neither the manifest's argv records nor the cmdline reached this init"

    # The workdir, applied AFTER the shares exist (the manifest's
    # chdir record — the cmdline route had /init.krun chdir BEFORE
    # the workspace share was mounted, silently landing at /).
    # PWD follows (bd myconfig-mr4): the workspace share is a bind
    # mount at its sandbox path, so getcwd() already reports it;
    # the export additionally pins the record's exact spelling when
    # a PARENT of the chdir is a symlink (the baked /mysbx-home).
    if [ -n "$manifest_chdir" ]; then
        step "chdir to the workspace: $manifest_chdir"
        cd "$manifest_chdir" \
            || fail "cannot chdir to the workdir $manifest_chdir"
        export PWD="$manifest_chdir"
    fi

    # The fd ceiling (bd myconfig-mnr): the payload execs as PID 1
    # of the VM with the kernel's default RLIMIT_NOFILE (soft 1024)
    # — on the bwrap backends it would inherit the host session's
    # limits, here nothing raises them, and a big-repo libgit2 walk
    # (nix flake check on the workspace share) dies with EMFILE at
    # 1024. Raise the hard ceiling first (the soft one cannot exceed
    # it), then the soft, best-effort: the payload must still run on
    # a kernel with a smaller nr_open, so a refusal is a WARNING plus
    # whatever limit stands, never a dead guest.
    # ulimit is an ash BUILTIN (not a busybox applet), so the
    # calls go to the shell itself — no $BB prefix here. As the VM's
    # root the hard raise reaches nr_open (1048576); a constrained
    # kernel keeps its own hard ceiling, and the soft then goes to
    # THAT — still a raise, never the 1024 default.
    nofile_target=1048576
    ulimit -Hn "$nofile_target" 2>/dev/null \
        || "$BB" echo "mysbx-init: WARNING: cannot raise the hard fd limit to $nofile_target (keeping $(ulimit -Hn))"
    if ulimit -n "$nofile_target" 2>/dev/null; then
        step "fd ceiling raised to $(ulimit -n)"
    elif ulimit -n "$(ulimit -Hn)" 2>/dev/null; then
        "$BB" echo "mysbx-init: WARNING: fd ceiling capped at the hard limit $(ulimit -n)"
    else
        "$BB" echo "mysbx-init: WARNING: cannot raise the soft fd limit (keeping $(ulimit -n))"
    fi

    # The store-share degradation probe (bd myconfig-mxu): the
    # payload's every shell spawn goes through bin/bash and bin/sh,
    # store symlinks valid only while the share carries their
    # targets — the live mxu failure had exactly these die mid-
    # session (spawn /bin/bash ENOENT, every command after it).
    # The init cannot fix a degraded share (the fix is a VM
    # refresh), but it can refuse to exec a payload whose shell
    # already dangles, naming the cause — a launch-time ENOENT with
    # a diagnosis beats a session that dies on its first command
    # with none. This runs AFTER the store overlay (a scratch-
    # carried bash would resolve through the upper layer too).
    # The shell's test builtin follows symlinks: -x on a link
    # whose target is gone is false (the db reconcile's own
    # property — "[ -e ] is FALSE for a dangling symlink"), so
    # this needs no readlink.
    for probe in /bin/bash /bin/sh /usr/bin/env; do
        [ -x "$probe" ] \
            || fail "the store share lost $probe — the baked symlink's target does not resolve; refresh the sandbox (the mxu degradation)"
    done

    # The fd-pin watchdog (bd myconfig-dj4): every guest inode
    # reached through a virtiofs share pins one host O_PATH fd in
    # the launcher (the FUSE lookup ref), released only on the
    # guest's FORGET — and this VM is never under memory pressure,
    # so its inode cache grows monotonically until the HOST hits
    # EMFILE (three live wedges at the raised 524288 ceiling). The
    # eviction lever is here, guest-side: past a threshold, drop
    # reclaimable dentries+inodes (drop_caches=2 keeps the page-
    # cache); the kernel then sends the FORGETs and the host fds
    # fall with them. Measured live: find /nix/store -maxdepth 2
    # pins 269k inodes, one echo 2 reclaims them all.
    # The loop rides a plain subshell fork of THIS init: it survives
    # the exec below, dies with the VM (PID 1 exit) and needs no
    # daemon, timer or extra store path. Both knobs ride the manifest
    # env route:
    # - MYSBX_KRUN_INODE_WATCHDOG (default 20000, 0 disables): the
    #   allowed growth above the post-drop baseline. The baseline is
    #   the inode count right after the last drop (0 before the
    #   first), so the in-use set that a drop cannot reclaim does not
    #   retrigger a drop every tick.
    # - MYSBX_KRUN_INODE_WATCHDOG_INTERVAL (default 1 s): the check
    #   interval. A walk pins fds between two checks, so the interval
    #   bounds the overshoot (bd myconfig-jjfy).
    # Every drop appends a line to /tmp/mysbx-inode-watchdog.log
    # (rotated to .1 past 256 KiB).
    if [ "''${MYSBX_KRUN_INODE_WATCHDOG:-20000}" != "0" ]; then
        step "arming the inode watchdog (growth ''${MYSBX_KRUN_INODE_WATCHDOG:-20000} over the post-drop baseline, interval ''${MYSBX_KRUN_INODE_WATCHDOG_INTERVAL:-1}s)"
        (
            _iw_limit="''${MYSBX_KRUN_INODE_WATCHDOG:-20000}"
            case "$_iw_limit" in
                "" | *[!0-9]*) _iw_limit=20000 ;;
            esac
            _iw_interval="''${MYSBX_KRUN_INODE_WATCHDOG_INTERVAL:-1}"
            case "$_iw_interval" in
                "" | *[!0-9]*) _iw_interval=1 ;;
            esac
            [ "$_iw_interval" -ge 1 ] 2>/dev/null || _iw_interval=1
            _iw_log=/tmp/mysbx-inode-watchdog.log
            _iw_base=0
            echo "armed: growth $_iw_limit, interval ''${_iw_interval}s" 2>/dev/null >> "$_iw_log" || :
            while :; do
                # inode-state is TAB-separated; read's default IFS
                # splits on any whitespace, so the first field alone
                # lands in _iw_inodes.
                read _iw_inodes _ < /proc/sys/fs/inode-state 2>/dev/null || _iw_inodes=0
                case "$_iw_inodes" in
                    "" | *[!0-9]*) _iw_inodes=0 ;;
                esac
                if [ "$_iw_inodes" -gt "$((_iw_base + _iw_limit))" ]; then
                    # `|| :` throughout: the init runs under set -e,
                    # and a failing command would end this loop.
                    "$BB" sync || :
                    echo 2 > /proc/sys/vm/drop_caches 2>/dev/null || :
                    read _iw_base _ < /proc/sys/fs/inode-state 2>/dev/null || _iw_base=0
                    case "$_iw_base" in
                        "" | *[!0-9]*) _iw_base=0 ;;
                    esac
                    read _iw_up _ < /proc/uptime 2>/dev/null || _iw_up=?
                    _iw_size=$("$BB" wc -c 2>/dev/null < "$_iw_log" || :)
                    _iw_size=''${_iw_size##* }
                    if [ "''${_iw_size:-0}" -gt 262144 ] 2>/dev/null; then
                        "$BB" mv -f "$_iw_log" "$_iw_log.1" 2>/dev/null || :
                    fi
                    echo "[''${_iw_up}s] drop: inodes $_iw_inodes -> $_iw_base, next drop above $((_iw_base + _iw_limit))" 2>/dev/null >> "$_iw_log" || :
                fi
                "$BB" sleep "$_iw_interval"
            done
        ) &
    fi

    # The console size (bd myconfig-a2uq): the qemu guest's serial
    # console is a 16550 — it carries NO winsize, so the guest ctty
    # reports a 0x0 grid and herdr refuses to attach (fish renders
    # blind). The pipeline read the CALLER's terminal size and rode
    # it through the manifest env; this stty applies it to the
    # guest console before the payload runs, so interactive payloads
    # see a real grid. Gated on the marker: the krun guest (libkrun's
    # console proxy carries a real size) and piped runs never set it.
    # The 16550 has no resize signaling either — the size is the
    # boot-time snapshot; a host resize mid-session does not
    # propagate (recorded in backends.md D9).
    _csize="''${MYSBX_MICROVM_CONSOLE_SIZE:-}"
    case "$_csize" in
        x*|*x*x|*x) ;;
        [0-9]*x[0-9]*)
            _rows=''${_csize%%x*}
            _cols=''${_csize#*x}
            case "$_rows$_cols" in
                *[!0-9]*) ;;
                *)
                    if [ "$_rows" -ge 1 ] && [ "$_cols" -ge 1 ]; then
                        "$BB" stty rows "$_rows" cols "$_cols" 2>/dev/null || :
                    fi
                    ;;
            esac
            ;;
    esac

    step "exec-ing the payload: $1"
    if [ -n "$exit_channel" ]; then
        # The exit channel (the qemu bridge): the payload runs as a
        # CHILD so this PID 1 can observe its exit code, hand it to
        # the launcher's reader (one "N\n" line on the virtio-serial
        # port), and exit — the kernel panics on PID-1 exit and
        # qemu's -no-reboot turns that panic into a clean exit.
        #
        # The port's DEVICE NODE is devtmpfs's own (the driver's
        # device_create names it vport<pdev>p<id>), but the FRIENDLY
        # /dev/virtio-ports/<name> spelling is a UDEV symlink — the
        # driver generates the udev event itself (virtio_console.c:
        # "Generate a udev event so that appropriate symlinks can be
        # created based on udev rules"), and this static-busybox
        # guest runs no udev. Resolve the port by NAME through sysfs
        # instead (bd myconfig-dnmk): walk /sys/class/virtio-ports/*,
        # match the name file, and write to the /dev/vport* node the
        # class dir names (devtmpfs already created it — a fallback
        # mknod covers a guest whose /dev is not devtmpfs). A failed
        # resolution or write falls through to the same teardown: the
        # launcher reports no code rather than hanging the VM.
        _exit_port=/dev/virtio-ports/$exit_channel
        for _port_dir in /sys/class/virtio-ports/*; do
            [ "$("$BB" cat "$_port_dir/name" 2>/dev/null)" = "$exit_channel" ] || continue
            _dev="$("$BB" cat "$_port_dir/dev" 2>/dev/null)" || continue
            _node=/dev/"$("$BB" basename "$_port_dir")"
            if [ -e "$_node" ]; then
                _exit_port=$_node
            elif "$BB" mknod "$_node" c "''${_dev%%:*}" "''${_dev#*:}" 2>/dev/null; then
                _exit_port=$_node
            fi
            break
        done
        # SYNCHRONOUS, never `cmd &`: a NON-INTERACTIVE ash gives an
        # async child /dev/null as stdin (the POSIX async-list rule —
        # live probe with this very busybox 1.37.0: the &-child read
        # EOF, the synchronous form read the piped line, bd
        # myconfig-8j3t), so a shell session or a piped-stdin payload
        # would see EOF the moment it starts. wait already blocks
        # this init until the payload exits — the synchronous form
        # reaps it just the same, keeps the console's stdin for the
        # payload, and $? carries the code.
        #
        # The CONTROLLING TERMINAL (bd myconfig-w8jd): PID 1 owns the
        # session; a plain child inherits the fds but never a ctty —
        # /dev/tty is ENXIO, job control is off, and ^C reaches no
        # foreground group. `setsid -c` execs the payload as a NEW
        # session leader WITH the console as its ctty (live-probed:
        # from ash the child is not a group leader, so setsid execs
        # in place and $? still carries the payload's code; a
        # non-tty stdin makes TIOCSCTTY fail silently and the
        # payload simply runs on, so piped one-shot runs are
        # unaffected).
        set +e
        "$BB" setsid -c "$@"
        _exit_code=$?
        set -e
        step "the payload exited with $_exit_code"
        printf '%s\n' "$_exit_code" > "$_exit_port" \
            || echo "mysbx-init: cannot write the exit code to the $exit_channel port ($_exit_port)" >&2
        exit 0
    fi
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
