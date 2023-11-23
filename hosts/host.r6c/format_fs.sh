#!/usr/bin/env bash

exit 2

# see: https://nixos.wiki/wiki/NixOS_on_ARM/NanoPi-R6C
set -euo pipefail

format_all() {
  DEV="${1}"
  wipefs -a "${DEV}"
  parted -s --align optimal "${DEV}" -- mklabel gpt;
  parted -s --align optimal "${DEV}" -- mkpart 'EFI'  2MB   6GiB  set 1 esp on;
  parted -s --align optimal "${DEV}" -- mkpart 'SWAP' 6GiB  16GiB;
  parted -s --align optimal "${DEV}" -- mkpart 'ROOT' 16GiB '100%';
  parted -s --align optimal "${DEV}" -- print;
  mkswap    "${DEV}p2";
  mkfs.vfat "${DEV}p1";
  mkfs.ext4 "${DEV}p3";
}

mount_all(){
  DEV="${1}"
  swapon "${DEV}p2"
  mount  /dev/nvme0n1p3 /mnt
  mkdir "${DEV}p3"
  mount  "${DEV}p1" /mnt/boot
}


format_all "$1"
mount_all "$1"
