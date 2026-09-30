#!/usr/bin/env bash
# krun-direct-spike — the live half of bd myconfig-dak.1: run the
# direct-libkrun launcher under bwrap on a host with /dev/kvm (f13)
# and answer the spike's questions:
#
#   1. does libkrun work under bwrap (KVM fd, no uid-0 mapping)?
#   2. does the guest kernel mount the extra virtiofs tags (spike-init
#      mounts them) and can a payload run a toolchain STRAIGHT from the
#      host /nix/store share, with no OCI image?
#   3. does the payload's exit code reach the caller?
#   4. boot time vs podman-krun.
#
# Each probe prints PASS/FAIL and exits non-zero on the first FAIL.
# Run from the repo checkout on f13:
#
#   ./nix/krun-direct-spike.sh
#
# The script is deliberately dumb: bash + nix build, no root, no
# switch. It builds the launcher (./krun-launcher.nix) and the spike
# rootfs (./krun-spike-rootfs.nix) from THIS checkout.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
# <root>/modules/myconfig.ai.dev/mysbx/nix/ -> four levels up is the
# flake root (the getFlake below needs it; the nix files are reached
# by absolute path, so a dirty checkout is fine).
repo=$(cd "$here/../../.."/.. && pwd)

fail() {
    printf 'FAIL %s — %s\n' "$1" "$2" >&2
    exit 1
}

pass() {
    printf 'PASS %s\n' "$1"
}

launcher=$(nix build --impure --no-link --print-out-paths --expr '
  (builtins.getFlake "git+file://'"$repo"'").inputs.nixpkgs.legacyPackages.x86_64-linux.callPackage
    '"$repo"'/modules/myconfig.ai.dev/mysbx/nix/krun-launcher.nix { }')

rootfs=$(nix build --impure --no-link --print-out-paths --expr '
  let
    np = (builtins.getFlake "git+file://'"$repo"'").inputs.nixpkgs.legacyPackages.x86_64-linux;
  in
  np.callPackage
    '"$repo"'/modules/myconfig.ai.dev/mysbx/nix/krun-spike-rootfs.nix {
      # The STATIC busybox of the guest entry — pkgsStatic, not the
      # dynamic busybox of the package set.
      busyboxStatic = np.pkgsStatic.busybox;
    }')

work=$(mktemp -d /tmp/mysbx-krun-spike.XXXXXX)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/repo"

# The bwrap chain of the epic's design: the launcher sees ONLY the
# rootfs, the host store, the payload dir and /dev/kvm. bwrap's uid
# mapping (no uid 0) is part of the probe — the VMM must run fine as
# the mapped user.
run() {
    bwrap \
        --ro-bind /nix/store /nix/store \
        --ro-bind "$rootfs" "$rootfs" \
        --bind "$work" "$work" \
        --dev-bind /dev/kvm /dev/kvm \
        --proc /proc \
        --clearenv \
        "$launcher/bin/mysbx-krun" "$@"
}

# 1. boot + toolchain from the host store, no OCI image (the epic's
# core claim): share virtiofs tags on the guest tmpfs, because virtiofs
# cannot be nested below the root virtiofs mount.
run \
    --rootfs "$rootfs" \
    --init /bin/spike-init \
    --ro-share "store@/tmp/mysbx-shares/store=/nix/store" \
    --rw-share "repo@/tmp/mysbx-shares/repo=$work/repo" \
    -- /nix/store/*-coreutils-*/bin/true
pass "1 boot: the payload ran /nix/store/.../true from the host store"

# 2. ro/rw enforcement: a write to the ro store share must fail, a
# write through the rw repo share must reach the host dir.
if run \
    --rootfs "$rootfs" \
    --init /bin/spike-init \
    --ro-share "store@/tmp/mysbx-shares/store=/nix/store" \
    --rw-share "repo@/tmp/mysbx-shares/repo=$work/repo" \
    -- /bin/sh -c ': > /tmp/mysbx-shares/store/PROBE' 2>/dev/null; then
    fail "2 ro share" "a write to the read-only /nix/store share succeeded"
fi
pass "2 ro share: the store share refused the write"

run \
    --rootfs "$rootfs" \
    --init /bin/spike-init \
    --ro-share "store@/tmp/mysbx-shares/store=/nix/store" \
    --rw-share "repo@/tmp/mysbx-shares/repo=$work/repo" \
    -- /bin/sh -c 'touch /tmp/mysbx-shares/repo/marker'
[ -e "$work/repo/marker" ] || fail "2 rw share" "the write through the rw share did not reach the host"
pass "2 rw share: the repo write reached the host directory"

# 3. exit code propagation (the spike's core question 3): libkrun's
# implicit init maps the workload exit onto KRUN_EXIT_CODE_IOCTL and
# krun_start_enter returns it to this shell.
rc=0
run \
    --rootfs "$rootfs" \
    --init /bin/spike-init \
    --ro-share "store@/tmp/mysbx-shares/store=/nix/store" \
    -- /bin/sh -c 'exit 42' || rc=$?
[ "$rc" -eq 42 ] || fail "3 exit code" "payload exit 42 arrived as $rc"
pass "3 exit code propagation: 42"

# 4. boot time: one one-shot run, wall clock, compared with a
# podman-krun one-shot of the same repo (both cold caches if run
# back-to-back after a reboot; on a warm host this is a lower bound).
start=$(date +%s%N)
run \
    --rootfs "$rootfs" \
    --init /bin/spike-init \
    --ro-share "store@/tmp/mysbx-shares/store=/nix/store" \
    -- /bin/busybox true
end=$(date +%s%N)
ms=$(((end - start) / 1000000))
printf 'INFO 4 boot time: %s ms (direct libkrun, one-shot true)\n' "$ms"
printf '     compare with: time mysbx run -- true   (backend = "podman-krun")\n'
pass "4 boot time recorded"

printf 'krun-direct-spike: all probes passed\n'
