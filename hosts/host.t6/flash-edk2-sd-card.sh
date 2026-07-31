#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Write the NanoPC-T6 EDK2/UEFI firmware image to a microSD card or eMMC
# (README.md steps 1-2).
#
# Unlike the ODROID-C2 (which boots a single self-built NixOS SD image), the
# NanoPC-T6 can be brought up through a board-specific EDK2/UEFI firmware image
# on microSD.  That microSD stays inserted permanently because it supplies the
# board firmware on every boot; NixOS itself is installed separately onto a
# target disk (eMMC or NVMe) from the AArch64 installer ISO (built via
# ../../build-iso-image.sh) and format-target-disk.sh.
#
# The EDK2 image (`nanopc-t6_UEFI_Release_v1.1.img`) is NOT built by this
# flake.  If no image path is given (and EDK2_IMG is unset) it is downloaded
# automatically from the edk2-porting/edk2-rk3588 release below.  The image is
# a plain .img (not compressed).  Alternatively, download a release yourself
# and pass its path here (or via the EDK2_IMG environment variable).
#
# Usage:
#   ./flash-edk2-sd-card.sh <device>              # auto-download firmware
#   ./flash-edk2-sd-card.sh <edk2-image> <device>
#   EDK2_IMG=nanopc-t6_UEFI_Release_v1.1.img ./flash-edk2-sd-card.sh /dev/sdX
#
# Overrides (environment variables):
#   EDK2_IMG   path to a pre-downloaded nanopc-t6_UEFI_Release_v1.1.img
#   EDK2_URL   download URL for the firmware image
#
# References:
#   https://github.com/edk2-porting/edk2-rk3588/releases
#   hosts/host.t6/README.md

set -euo pipefail

# Default firmware image to fetch when none is supplied on the command line.
EDK2_URL="${EDK2_URL:-https://github.com/edk2-porting/edk2-rk3588/releases/download/v1.1/nanopc-t6_UEFI_Release_v1.1.img}"

IMG="${EDK2_IMG:-}"
DEVICE=""
WORKDIR=""

# Clean up any temp dir created for an auto-downloaded image.
cleanup() {
    [[ -n ${WORKDIR} && -d ${WORKDIR} ]] && rm -rf -- "${WORKDIR}"
}
trap cleanup EXIT

usage() {
    cat >&2 <<EOF
Usage: $0 [<edk2-image>] <device>

  <edk2-image>   Path to nanopc-t6_UEFI_Release_v1.1.img (or set EDK2_IMG).
                 If omitted, the firmware is downloaded automatically from:
                   ${EDK2_URL}
  <device>       microSD/eMMC block device to flash (e.g. /dev/sdX, /dev/mmcblk0).

The whole target device is overwritten. Double-check the device name with
\`lsblk\` before running. Leave this microSD inserted during every boot: it
provides the board firmware.
EOF
    exit 2
}

# Download EDK2_URL into a temp dir.  The NanoPC-T6 image is a plain .img
# (no gzip), but we still handle .gz gracefully in case an upstream release
# changes its packaging.  Echo the resulting image path.
download_image() {
    local url="$1" dst
    WORKDIR="$(mktemp -d)"
    dst="${WORKDIR}/${url##*/}"
    echo "==> Downloading EDK2 firmware from ${url} ..." >&2
    if command -v curl >/dev/null 2>&1; then
        curl -fL --progress-bar -o "${dst}" "${url}"
    elif command -v wget >/dev/null 2>&1; then
        wget -O "${dst}" "${url}"
    else
        echo "error: neither curl nor wget is available to download ${url}" >&2
        exit 1
    fi
    if [[ ${dst} == *.gz ]]; then
        echo "==> Decompressing ${dst##*/} ..." >&2
        gunzip -f "${dst}"
        dst="${dst%.gz}"
    fi
    echo "${dst}"
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

if [[ -z ${DEVICE} ]]; then
    echo "error: no target device given" >&2
    usage
fi

if [[ ! -b ${DEVICE} ]]; then
    echo "error: ${DEVICE} is not a block device" >&2
    exit 1
fi

# Only download after the device has been validated, to avoid a wasted fetch.
if [[ -z ${IMG} ]]; then
    IMG="$(download_image "${EDK2_URL}")"
fi

if [[ ! -f ${IMG} ]]; then
    echo "error: EDK2 image not found: ${IMG}" >&2
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
