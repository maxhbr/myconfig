# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The Nix userspace of the podman-krun guest (bd myconfig-pz6,
# docs/design/backends.md D2): one tree of `bin/nix*` wrappers around
# `nix`, baked into the (shared) agent image via
# `myconfig.ai.dev.mysbx.krun.nix.packages`.
#
# As guest root, the first wrapper invocation of a VM (under a lock on
# the guest's /dev/shm) mounts the nix scratch and copies the image's
# nix state (`/nix/var/nix`, from dockerTools `includeNixDB`) into
# it, then mounts an overlayfs over `/nix/store` whose lower layer is
# the image's OWN store and whose upper layer lives on the scratch.
# Nix then runs single-user against that store, with state, logs,
# cache and TMPDIR on the scratch.
#
# The scratch itself is disk-backed when the run provides one (bd
# myconfig-0pi): the host truncates a per-run sparse file, binds it
# over virtio-fs and names it in `MYSBX_KRUN_SCRATCH_IMG`; the setup
# loop-mounts it (losetup + mkfs.ext4 — the guest kernel's OWN
# filesystem: chown, overlay xattrs and whiteouts work natively,
# nothing is proxied over the virtiofs xattr surface) and REMOVES the
# path right after the attach: the open loop device keeps the inode
# alive, the space frees itself when the VM dies, and the host's
# next-run sweep never sees a live run's file. Without the variable
# the setup mounts a guest tmpfs instead — ANNOUNCED, never a silent
# switch (the refuse-or-announce rule of bd myconfig-0pi).
#
# Constraints, all live-validated on f13 (bd myconfig-pz6 probes; the
# disk scratch's own probes are bd myconfig-0pi's runbook):
#
# - Nothing is mounted before boot, and the lower layer is the store
#   the image's own /bin resolves through, so every image path keeps
#   resolving before, during and after the mount.
# - Copy-up of a lower entry fails: virtiofs has no fileattr support
#   (`overlayfs: failed to retrieve lower fileattr ... err=-95`). Image
#   paths are therefore never modified. They are registered valid, so
#   nix never replaces them. The database is copied, not overlaid, and
#   `nix store optimise` cannot work.
# - Nix state on virtio-fs fails: the virtiofs server forwards chown
#   unchanged (EPERM), and libgit2 refuses the host-uid-owned home
#   cache. Hence state, logs, cache and TMPDIR live on the scratch
#   (tmpfs when no disk was provided).
# - The image's `db/big-lock` and `db/reserved` are 0600 and owned by an
#   image uid that guest root cannot read through virtio-fs. They carry
#   no data (a lock file and reserved disk space) and nix recreates
#   both, so the copy skips them.
# - The tmpfs is RAM: it may take up to half the VM memory (tmpfs
#   default). The wrapper warns when the VM is smaller than `minRamMib`;
#   with a disk scratch that warning is informational only (the
#   scratch does not compete for VM RAM).
#
# As any other uid (podman-gvisor), the wrappers exec nix
# untouched.
{
  lib,
  runCommand,
  writeShellApplication,
  coreutils,
  e2fsprogs,
  util-linux,
  gawk,
  nix,
  scratch ? "/run/mysbx-nix",
  nixConfig ? "",
  minRamMib ? 4096,
}:
let
  # Every entry point of `nix/bin` except the daemon: nix dispatches on
  # the basename of argv[0], so each name needs its own wrapper.
  names = [
    "nix"
    "nix-build"
    "nix-channel"
    "nix-collect-garbage"
    "nix-copy-closure"
    "nix-env"
    "nix-hash"
    "nix-instantiate"
    "nix-prefetch-url"
    "nix-shell"
    "nix-store"
  ];

  defaultNixConfig = lib.concatStringsSep "\n" (
    [
      # No build users in the image (fakeNss), and no daemon.
      "build-users-group ="
      "sandbox = false"
      "experimental-features = nix-command flakes"
    ]
    ++ lib.optional (nixConfig != "") nixConfig
  );

  # Copies nix state dir SRC to DST, without `db/big-lock` and
  # `db/reserved`.
  copyState = writeShellApplication {
    name = "mysbx-krun-nix-copy-state";
    runtimeInputs = [ coreutils ];
    text = ''
      src=$1
      dst=$2
      mkdir -p "$dst/db"
      for f in "$src"/*; do
        [ -e "$f" ] || continue
        [ "''${f##*/}" != db ] || continue
        cp -R "$f" "$dst/"
      done
      for f in "$src"/db/*; do
        [ -e "$f" ] || continue
        case "''${f##*/}" in
          big-lock | reserved) continue ;;
        esac
        cp -R "$f" "$dst/db/"
      done
    '';
  };

  setup = writeShellApplication {
    name = "mysbx-krun-nix-setup";
    runtimeInputs = [
      coreutils
      util-linux
      gawk
      e2fsprogs
    ];
    text = ''
      scratch=${lib.escapeShellArg scratch}
      fail() {
        echo "nix (mysbx krun wrapper): $*" >&2
        echo "  the guest nix store is an overlay of the image store on the nix scratch (docs/design/backends.md D2)" >&2
        exit 125
      }
      { exec 9</dev/shm; } 2>/dev/null || fail "cannot open /dev/shm for the store lock"
      flock -w 60 9 || fail "cannot take the store lock on /dev/shm"
      if ! findmnt -rn -t overlay --mountpoint /nix/store >/dev/null; then
        [ -f /nix/var/nix/db/db.sqlite ] \
          || fail "the image carries no registered nix database at /nix/var/nix/db (includeNixDB)"
        # The scratch surface: the disk-backed per-run file when the
        # run provides one (MYSBX_KRUN_SCRATCH_IMG, bd
        # myconfig-0pi), a guest tmpfs otherwise. The tmpfs path
        # ANNOUNCES itself — a run that silently switched surfaces
        # would be a lie about where the state lives.
        img="''${MYSBX_KRUN_SCRATCH_IMG:-}"
        if [ -n "$img" ]; then
          [ -f "$img" ] || fail "the scratch image $img does not exist in the guest"
          # losetup --find: the guest kernel picks a free loop device
          # (no /dev/loop-control juggling). Raw block device, no
          # offset/partition — the host truncated the file to the
          # run's size cap and nothing else ever wrote it.
          loop=$(losetup --find --show "$img") \
            || fail "cannot attach $img to a loop device (the guest kernel needs the loop module)"
          # Remove the path IMMEDIATELY after the attach (bd
          # myconfig-0pi): the open loop device keeps the inode alive,
          # so the ext4 keeps working while nothing can re-open or
          # re-share the file — and the host's next-run sweep of
          # <sidecar>/scratch/ never mistakes a live run for debris.
          rm -f "$img" \
            || echo "nix (mysbx krun wrapper): warning: could not remove the scratch image path $img" >&2
          mkfs.ext4 -q -F "$loop" \
            || fail "cannot create an ext4 filesystem on $loop (the scratch disk is per-run; a leftover is impossible)"
          mkdir -p "$scratch" || fail "cannot create the scratch mountpoint $scratch"
          mount -t ext4 "$loop" "$scratch" \
            || fail "cannot mount the ext4 scratch $loop at $scratch"
          echo "nix (mysbx krun wrapper): the nix scratch is the disk-backed ext4 on $loop (bd myconfig-0pi)" >&2
        else
          echo "nix (mysbx krun wrapper): warning: no scratch disk provided (MYSBX_KRUN_SCRATCH_IMG unset) — the nix scratch is a GUEST TMPFS, new store paths cost VM RAM" >&2
          if ! findmnt -rn -t tmpfs --mountpoint "$scratch" >/dev/null; then
            { mkdir -p "$scratch" && mount -t tmpfs -o mode=0755 mysbx-nix "$scratch"; } \
              || fail "cannot mount a guest tmpfs at $scratch"
          fi
        fi
        rm -rf "$scratch/state" "$scratch/upper" "$scratch/work"
        mkdir -p "$scratch"/{upper,work,tmp,cache,log} || fail "cannot create the store dirs in $scratch"
        ${copyState}/bin/mysbx-krun-nix-copy-state /nix/var/nix "$scratch/state" \
          || fail "cannot copy the image nix database to $scratch/state"
        mount -t overlay mysbx-nix-store \
          -o "lowerdir=/nix/store,upperdir=$scratch/upper,workdir=$scratch/work" /nix/store \
          || fail "cannot mount the overlay over /nix/store"
        mem=$(awk '/^MemTotal:/ { print int($2 / 1024) }' /proc/meminfo)
        if [ -n "$img" ]; then
          # The disk-backed scratch does not compete for VM RAM; the
          # size warning is informational only (the VM still needs
          # RAM for builds themselves).
          if [ "''${mem:-0}" -lt ${toString minRamMib} ]; then
            echo "nix (mysbx krun wrapper): warning: the VM has ''${mem} MiB; the scratch is disk-backed, but builds themselves still need RAM." >&2
            echo "  set MYSBX_PODMAN_MEMORY (e.g. 8g) for substitutions and dev shells" >&2
          fi
        elif [ "''${mem:-0}" -lt ${toString minRamMib} ]; then
          echo "nix (mysbx krun wrapper): warning: the VM has ''${mem} MiB; new store paths live on tmpfs (at most half of it)." >&2
          echo "  set MYSBX_PODMAN_MEMORY (e.g. 8g) for substitutions and dev shells" >&2
        fi
      fi
      exec 9<&-
    '';
  };

  wrapper =
    name:
    writeShellApplication {
      inherit name;
      runtimeInputs = [ coreutils ];
      text = ''
        if [ "$(id -u)" -eq 0 ]; then
          ${setup}/bin/mysbx-krun-nix-setup
          scratch=${lib.escapeShellArg scratch}
          export NIX_REMOTE=local
          export NIX_STATE_DIR="$scratch/state"
          export NIX_LOG_DIR="$scratch/log"
          export NIX_CACHE_HOME="$scratch/cache"
          export TMPDIR="$scratch/tmp"
          # Host and user settings come after the defaults, so they win.
          defaults=${lib.escapeShellArg defaultNixConfig}
          nl=$'\n'
          export NIX_CONFIG="$defaults''${NIX_CONFIG:+$nl$NIX_CONFIG}"
        fi
        exec ${nix}/bin/${name} "$@"
      '';
    };
in
runCommand "mysbx-krun-guest-nix"
  {
    passthru = {
      inherit
        nix
        setup
        copyState
        names
        defaultNixConfig
        ;
    };
    # The image's buildEnv ignores collisions: the wrappers must win
    # against a plain `nix` in the same image.
    meta.priority = -10;
  }
  ''
    mkdir -p $out/bin
    ${lib.concatMapStringsSep "\n" (n: "ln -s ${wrapper n}/bin/${n} $out/bin/${n}") names}
  ''
