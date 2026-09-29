# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The Nix userspace of the podman-krun guest (bd myconfig-pz6,
# docs/design/backends.md D2): one tree of `bin/nix*` wrappers around
# `nix`, baked into the (shared) agent image via
# `myconfig.ai.dev.mysbx.krun.nix.packages`.
#
# As guest root, the first wrapper invocation of a VM (under a lock on
# the guest's /dev/shm) mounts a guest tmpfs at `scratch`. It copies the
# image's registered database (`/nix/var/nix`, from dockerTools
# `includeNixDB`) into that tmpfs, then mounts an overlayfs over
# `/nix/store` whose lower layer is the image's OWN store and whose
# upper layer lives on the tmpfs. Nix then runs single-user against
# that store, with state, logs, cache and TMPDIR on the tmpfs.
#
# Constraints, all live-validated on f13 (bd myconfig-pz6 probes):
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
#   cache. Hence state, logs, cache and TMPDIR live on the tmpfs.
# - The tmpfs is RAM: it may take up to half the VM memory (tmpfs
#   default). The wrapper warns when the VM is smaller than `minRamMib`.
#
# As any other uid (podman-gvisor, agent-gvisor), the wrappers exec nix
# untouched.
{
  lib,
  runCommand,
  writeShellApplication,
  coreutils,
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

  setup = writeShellApplication {
    name = "mysbx-krun-nix-setup";
    runtimeInputs = [
      coreutils
      util-linux
      gawk
    ];
    text = ''
      scratch=${lib.escapeShellArg scratch}
      fail() {
        echo "nix (mysbx krun wrapper): $*" >&2
        echo "  the guest nix store is an overlay of the image store on guest tmpfs (docs/design/backends.md D2)" >&2
        exit 125
      }
      { exec 9</dev/shm; } 2>/dev/null || fail "cannot open /dev/shm for the store lock"
      flock -w 60 9 || fail "cannot take the store lock on /dev/shm"
      if ! findmnt -rn -t overlay --mountpoint /nix/store >/dev/null; then
        [ -f /nix/var/nix/db/db.sqlite ] \
          || fail "the image carries no registered nix database at /nix/var/nix/db (includeNixDB)"
        if ! findmnt -rn -t tmpfs --mountpoint "$scratch" >/dev/null; then
          { mkdir -p "$scratch" && mount -t tmpfs -o mode=0755 mysbx-nix "$scratch"; } \
            || fail "cannot mount a guest tmpfs at $scratch"
        fi
        rm -rf "$scratch/state" "$scratch/upper" "$scratch/work"
        mkdir -p "$scratch"/{upper,work,tmp,cache,log} || fail "cannot create the store dirs in $scratch"
        cp -R /nix/var/nix "$scratch/state" || fail "cannot copy the image nix database to $scratch/state"
        mount -t overlay mysbx-nix-store \
          -o "lowerdir=/nix/store,upperdir=$scratch/upper,workdir=$scratch/work" /nix/store \
          || fail "cannot mount the overlay over /nix/store"
        mem=$(awk '/^MemTotal:/ { print int($2 / 1024) }' /proc/meminfo)
        if [ "''${mem:-0}" -lt ${toString minRamMib} ]; then
          echo "nix (mysbx krun wrapper): warning: the VM has ''${mem} MiB; new store paths live on tmpfs (at most half of it)." >&2
          echo "  set MYSBX_GVISOR_MEMORY (e.g. 8g) for substitutions and dev shells" >&2
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
        names
        defaultNixConfig
        ;
    };
    # The image's buildEnv ignores collisions: the wrappers must win
    # against a plain `nix` in the same image (the gvisor tier's
    # `nix.enable`).
    meta.priority = -10;
  }
  ''
    mkdir -p $out/bin
    ${lib.concatMapStringsSep "\n" (n: "ln -s ${wrapper n}/bin/${n} $out/bin/${n}") names}
  ''
