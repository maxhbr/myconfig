#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Write the NixOS AArch64 minimal installer ISO to a USB stick
# (README.md step 3).
#
# EDK2 exposes the ROC-RK3568-PC as a UEFI AArch64 system, so the regular
# upstream NixOS installer ISO boots via EFI/BOOT/BOOTAA64.EFI.  This flake
# does not build that ISO; download the current minimal aarch64 image from:
#   https://nixos.org/download/#nixos-iso
# (file name similar to `nixos-minimal-<release>-aarch64-linux.iso`) and pass
# its path here (or via the NIXOS_ISO environment variable).
#
# Usage:
#   ./flash-installer-usb.sh <iso> <device>
#   NIXOS_ISO=nixos-minimal-25.05-aarch64-linux.iso ./flash-installer-usb.sh /dev/sdY
#
# References:
#   https://wiki.nixos.org/wiki/NixOS_on_ARM/UEFI
#   hosts/host.roc/README.md

set -euo pipefail

ISO="${NIXOS_ISO:-}"
DEVICE=""

usage() {
    cat >&2 <<EOF
Usage: $0 [<iso>] <device>

  <iso>      Path to a NixOS aarch64 minimal ISO (or set NIXOS_ISO).
  <device>   USB block device to flash (e.g. /dev/sdY).

Download a current minimal aarch64 ISO from:
  https://nixos.org/download/#nixos-iso

The whole target device is overwritten. Double-check the device name with
\`lsblk\` before running.
EOF
    exit 2
}

for arg in "$@"; do
    case "$arg" in
        -h | --help) usage ;;
        -*) echo "unknown option: $arg" >&2 && usage ;;
        *)
            if [[ -z ${ISO} && ! -b ${arg} ]]; then
                ISO="$arg"
            else
                DEVICE="$arg"
            fi
            ;;
    esac
done

if [[ -z ${ISO} ]]; then
    echo "error: no ISO given (pass a path or set NIXOS_ISO)" >&2
    usage
fi

if [[ ! -f ${ISO} ]]; then
    echo "error: ISO not found: ${ISO}" >&2
    exit 1
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
echo "==> ISO: ${ISO} ($(du -h "${ISO}" | cut -f1))" >&2
echo >&2
read -r -p "About to OVERWRITE ${DEVICE} with ${ISO##*/}. Type 'yes' to continue: " ans
if [[ ${ans} != "yes" ]]; then
    echo "aborted." >&2
    exit 1
fi

echo "==> Unmounting any mounted partitions of ${DEVICE} ..." >&2
for part in "${DEVICE}"?*; do
    [[ -b ${part} ]] && sudo umount "${part}" 2>/dev/null || true
done

echo "==> Writing installer ISO to ${DEVICE} ..." >&2
sudo dd if="${ISO}" of="${DEVICE}" bs=4M conv=fsync status=progress

echo "==> Syncing ..." >&2
sync

echo "==> Done. Boot the board with the EDK2 microSD + this USB stick inserted." >&2
