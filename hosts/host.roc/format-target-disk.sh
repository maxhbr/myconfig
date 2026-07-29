#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Partition, format and mount the NixOS target disk for the ROC-RK3568-PC
# (README.md step 7).  Run this FROM the booted NixOS aarch64 installer, as
# root, before `nixos-generate-config --root /mnt` and `nixos-install`.
#
# It creates a GPT with a 512 MiB EFI System Partition (FAT32, label "EFI")
# and an ext4 root partition (label "nixos"), matching the placeholder
# by-label devices in hosts/host.roc/hardware-configuration.nix, then mounts
# them at /mnt and /mnt/boot.
#
# NVMe is the preferred target because it leaves the vendor eMMC untouched.
#
# Usage:
#   sudo ./format-target-disk.sh <device>          # e.g. /dev/nvme0n1
#
# References:
#   hosts/host.roc/README.md

set -euo pipefail

DISK=""

usage() {
    cat >&2 <<EOF
Usage: $0 <device>

  <device>   Whole-disk target for NixOS (e.g. /dev/nvme0n1, /dev/sda).

NVMe is preferred so the vendor eMMC installation stays untouched.

The whole target device is wiped. Verify the device carefully with:
  lsblk -o NAME,SIZE,MODEL,SERIAL,TYPE,TRAN,MOUNTPOINTS
EOF
    exit 2
}

for arg in "$@"; do
    case "$arg" in
        -h | --help) usage ;;
        -*) echo "unknown option: $arg" >&2 && usage ;;
        *) DISK="$arg" ;;
    esac
done

if [[ -z ${DISK} ]]; then
    echo "error: no target device given" >&2
    usage
fi

if [[ ! -b ${DISK} ]]; then
    echo "error: ${DISK} is not a block device" >&2
    exit 1
fi

if [[ $EUID -ne 0 ]]; then
    echo "error: must run as root (partitioning/formatting/mounting)" >&2
    exit 1
fi

# NVMe and eMMC devices use `pN` partition suffixes (e.g. nvme0n1p1); SATA and
# USB disks such as /dev/sda use plain `N` (e.g. sda1).
if [[ ${DISK} =~ (nvme[0-9]+n[0-9]+|mmcblk[0-9]+)$ ]]; then
    ESP="${DISK}p1"
    ROOT="${DISK}p2"
else
    ESP="${DISK}1"
    ROOT="${DISK}2"
fi

echo >&2
lsblk "${DISK}" >&2 || true
echo >&2
echo "==> Will create on ${DISK}:" >&2
echo "      ${ESP}  512 MiB  FAT32  ESP   (label EFI)" >&2
echo "      ${ROOT} rest     ext4   root  (label nixos)" >&2
echo >&2
read -r -p "About to WIPE ${DISK}. Type 'yes' to continue: " ans
if [[ ${ans} != "yes" ]]; then
    echo "aborted." >&2
    exit 1
fi

echo "==> Wiping existing signatures on ${DISK} ..." >&2
wipefs -a "${DISK}"

echo "==> Creating GPT partition table ..." >&2
parted "${DISK}" --script \
    mklabel gpt \
    mkpart ESP fat32 1MiB 513MiB \
    set 1 esp on \
    mkpart nixos ext4 513MiB 100%

# Give the kernel a moment to pick up the new partition nodes.
sleep 1
udevadm settle 2>/dev/null || true

echo "==> Formatting ${ESP} (FAT32, label EFI) ..." >&2
mkfs.fat -F 32 -n EFI "${ESP}"

echo "==> Formatting ${ROOT} (ext4, label nixos) ..." >&2
mkfs.ext4 -L nixos "${ROOT}"

echo "==> Mounting root at /mnt and ESP at /mnt/boot ..." >&2
mount "${ROOT}" /mnt
mkdir -p /mnt/boot
mount "${ESP}" /mnt/boot

echo "==> Done. Next:" >&2
echo "      nixos-generate-config --root /mnt" >&2
echo "      # review generated hardware-configuration.nix, then install" >&2
echo "      nixos-install --flake .#roc" >&2
