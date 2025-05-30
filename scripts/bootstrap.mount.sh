#!/usr/bin/env bash

set -euo pipefail

LUKS_UUID="$1"
BTRFS_UUID="$2"
BOOT_UUID="$3"

CRYPT_NAME="enc-pv"
LUKS_DEV="/dev/disk/by-uuid/${LUKS_UUID}"

MNT="/mnt"

if ! cryptsetup status "$CRYPT_NAME" &>/dev/null; then
  cryptsetup open "$LUKS_DEV" "$CRYPT_NAME" --allow-discards
fi

mkdir -p "$MNT"
mountpoint -q "$MNT" || \
  mount -t tmpfs -o size=25%,mode=755 none "$MNT"

mount_btrfs() {
  local subvol="$1" mntpt="$2"
  mkdir -p "$mntpt"
  if ! mountpoint -q "$mntpt"; then
    mount -o "subvol=${subvol}" "UUID=${BTRFS_UUID}" "$mntpt"
  fi
}

mount_btrfs "@nix"             "$MNT/nix"
mount_btrfs "@log"             "$MNT/var/log"
mount_btrfs "@home"            "$MNT/home"
mount_btrfs "@persistent_priv" "$MNT/persistent/priv"
mount_btrfs "@persistent_work" "$MNT/persistent/work"
mount_btrfs "@persistent_cache" "$MNT/persistent/cache"
mount_btrfs "@swapfile"        "$MNT/.swapfile"

mkdir -p "$MNT/boot"
mountpoint -q "$MNT/boot" || \
  mount "UUID=${BOOT_UUID}" "$MNT/boot"
