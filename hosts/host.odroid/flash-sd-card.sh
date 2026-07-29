#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Build the ODROID-C2 (host "odroid") SD image and flash it to an SD card / eMMC.
#
# This script builds against the flake in the *current working directory*, and
# targets the real `nixosConfigurations.odroid` system.  In this repo's setup
# the actual system (including agenix secrets) is assembled by a separate
# private flake (../priv) which imports this public flake and exposes the
# non-test host configurations.  Therefore run this script from the root of
# that flake, e.g.:
#
#   cd ~/myconfig/priv && ~/myconfig/myconfig/hosts/host.odroid/flash-sd-card.sh /dev/sdX
#
# Overrides (environment variables):
#   FLAKE   flake reference to build from        (default ".")
#   TARGET  nixosConfigurations attribute name    (default "odroid")
#   NIX_BUILD_EXTRA_ARGS  extra args for `nix build` (e.g. "--builders ...")
#
# The built image already has the board-specific U-Boot fused into the raw gap
# in front of the first partition (see hosts/host.odroid/hardware-configuration.nix,
# sdImage.postBuildCommands), so a plain `dd` of the whole image is bootable.
#
# Building an aarch64 image on an x86_64 host requires either an aarch64 remote
# builder or binfmt/qemu emulation:
#   boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
#
# Usage:
#   ./flash-sd-card.sh /dev/sdX          # build + flash to /dev/sdX
#   ./flash-sd-card.sh --build-only      # only build, print image path
#
# References:
#   https://nixos.wiki/wiki/NixOS_on_ARM/ODROID-C2

set -euo pipefail

FLAKE="${FLAKE:-.}"
TARGET="${TARGET:-odroid}"
FLAKE_ATTR="${FLAKE}#nixosConfigurations.${TARGET}.config.system.build.sdImage"

usage() {
    cat >&2 <<EOF
Usage: $0 [--build-only] [<device>]

  <device>       Block device to flash (e.g. /dev/sdX, /dev/mmcblk0).
  --build-only   Build the SD image only and print its store path.

Run from the root of the flake that provides nixosConfigurations.${TARGET}
(the private flake that adds secrets), not from the public myconfig flake
(which only exposes the secret-less test-${TARGET} configuration).

The whole target device is overwritten. Double-check the device name with
\`lsblk\` before running.
EOF
    exit 2
}

BUILD_ONLY=0
DEVICE=""
for arg in "$@"; do
    case "$arg" in
        --build-only) BUILD_ONLY=1 ;;
        -h | --help) usage ;;
        -*) echo "unknown option: $arg" >&2 && usage ;;
        *) DEVICE="$arg" ;;
    esac
done

# We build against the flake in the current working directory. Sanity-check
# that a flake actually lives here so a mistaken pwd fails early and loudly.
if [[ ${FLAKE} == "." && ! -e "./flake.nix" ]]; then
    echo "error: no flake.nix in $(pwd)" >&2
    echo "       run this from the root of the flake providing nixosConfigurations.${TARGET}" >&2
    exit 1
fi

# Extra `nix build` args (e.g. `--builders ...`) can be passed via the
# NIX_BUILD_EXTRA_ARGS environment variable (word-split intentionally).
read -r -a extra_args <<<"${NIX_BUILD_EXTRA_ARGS:-}"

# Follow the repo convention of placing result symlinks next to the flake root.
OUT_LINK="../result.${TARGET}.sd-image"

echo "==> Updating flake inputs ..." >&2
nix flake update

echo "==> Building SD image for '${TARGET}' from flake '${FLAKE}' ..." >&2
nix build --print-out-paths "${FLAKE_ATTR}" \
    --out-link "${OUT_LINK}" \
    "${extra_args[@]}" >&2

OUT="$(nix path-info "${FLAKE_ATTR}")"
IMG="$(find "${OUT}/sd-image" -name '*.img' | head -n1)"

if [[ -z ${IMG} || ! -f ${IMG} ]]; then
    echo "error: could not locate built .img under ${OUT}/sd-image" >&2
    exit 1
fi

echo "==> Built image: ${IMG}" >&2

if [[ ${BUILD_ONLY} -eq 1 ]]; then
    echo "${IMG}"
    exit 0
fi

if [[ -z ${DEVICE} ]]; then
    echo "error: no target device given" >&2
    usage
fi

if [[ ! -b ${DEVICE} ]]; then
    echo "error: ${DEVICE} is not a block device" >&2
    exit 1
fi

echo >&2
lsblk "${DEVICE}" >&2 || true
echo >&2
read -r -p "About to OVERWRITE ${DEVICE} with ${IMG##*/}. Type 'yes' to continue: " ans
if [[ ${ans} != "yes" ]]; then
    echo "aborted." >&2
    exit 1
fi

echo "==> Unmounting any mounted partitions of ${DEVICE} ..." >&2
for part in "${DEVICE}"?*; do
    [[ -b ${part} ]] && sudo umount "${part}" 2>/dev/null || true
done

echo "==> Writing image to ${DEVICE} (this can take a while) ..." >&2
sudo dd if="${IMG}" of="${DEVICE}" bs=4M conv=fsync status=progress

echo "==> Syncing ..." >&2
sync

echo "==> Done. The root partition auto-expands on first boot." >&2
