#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Write the ROC-RK3568-PC EDK2/UEFI firmware image to a microSD card
# (README.md steps 1-2).
#
# Unlike the ODROID-C2 (which boots a single self-built NixOS SD image), the
# ROC-RK3568-PC is brought up through a board-specific EDK2/UEFI firmware image
# on microSD.  That microSD stays inserted permanently because it supplies the
# board firmware on every boot; NixOS itself is installed separately onto NVMe
# (see flash-installer-usb.sh and format-target-disk.sh).
#
# The EDK2 image (`ROC-RK3568-PC_EFI.img`) is NOT built by this flake.  Download
# a release from:
#   https://github.com/S199pWa1k9r/edk2-rk356x/releases
# and pass its path here (or via the EDK2_IMG environment variable).
#
# Usage:
#   ./flash-edk2-sd-card.sh <edk2-image> <device>
#   EDK2_IMG=ROC-RK3568-PC_EFI.img ./flash-edk2-sd-card.sh /dev/sdX
#
# References:
#   https://github.com/S199pWa1k9r/edk2-rk356x/blob/main/docs/firefly-ROC-RK356x-PC.md
#   hosts/host.roc/README.md

set -euo pipefail

IMG="${EDK2_IMG:-}"
DEVICE=""

usage() {
    cat >&2 <<EOF
Usage: $0 [<edk2-image>] <device>

  <edk2-image>   Path to ROC-RK3568-PC_EFI.img (or set EDK2_IMG).
  <device>       microSD block device to flash (e.g. /dev/sdX, /dev/mmcblk0).

Download the EDK2 firmware image from:
  https://github.com/S199pWa1k9r/edk2-rk356x/releases

The whole target device is overwritten. Double-check the device name with
\`lsblk\` before running. Leave this microSD inserted during every boot: it
provides the board firmware.
EOF
    exit 2
}

for arg in "$@"; do
    case "$arg" in
        -h | --help) usage ;;
        -*) echo "unknown option: $arg" >&2 && usage ;;
        *)
            if [[ -z ${IMG} && ! -b ${arg} ]]; then
                IMG="$arg"
            else
                DEVICE="$arg"
            fi
            ;;
    esac
done

if [[ -z ${IMG} ]]; then
    echo "error: no EDK2 image given (pass a path or set EDK2_IMG)" >&2
    usage
fi

if [[ ! -f ${IMG} ]]; then
    echo "error: EDK2 image not found: ${IMG}" >&2
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
echo "==> EDK2 image: ${IMG} ($(du -h "${IMG}" | cut -f1))" >&2
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

echo "==> Writing EDK2 firmware to ${DEVICE} ..." >&2
sudo dd if="${IMG}" of="${DEVICE}" bs=4M conv=fsync status=progress

echo "==> Syncing ..." >&2
sync

echo "==> Done. Keep this microSD inserted; it supplies the board firmware." >&2
