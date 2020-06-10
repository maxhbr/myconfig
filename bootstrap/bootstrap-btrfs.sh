#!/usr/bin/env nix-shell
#! nix-shell -i bash -p parted btrfsProgs utillinux
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see / based on:
# - https://nixos.org/nixos/manual/
# - https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134

set -e

################################################################################
##  functions  #################################################################
################################################################################

help() {
    cat <<EOF
usage:
  $ BOOTSTRAP=YES $0 /dev/SDX [mnt]
EOF
}

mkEfiPartitions() {
    parted $SDX -- mklabel gpt
    parted $SDX -- mkpart primary 512MiB 100%
    parted $SDX -- mkpart ESP fat32 1MiB 512MiB
    parted $SDX -- set 2 boot on
}

mkLegacyPartitions() {
    parted ${SDX} -- mklabel msdos
    parted ${SDX} -- mkpart primary 1MiB -8GiB
    parted ${SDX} -- mkpart primary linux-swap -8GiB 100%
}

mkSwap() {
    local swapDev="$1"

    mkswap -L swap "$swapDev"
    swapon "$swapDev"
}

formatRoot() {
    local rootDev="$1"

    mkfs.ext4 -L root "$rootDev"
}

formatBoot() {
    local bootDev="$1"

    mkfs.fat -F 32 -n boot "$bootDev"
}

mkBTRFS() {
    # see: https://gist.github.com/samdroid-apps/3723d30953af5e1d68d4ad5327e624c0
    local btrfsDev="$1"
    mkfs.btrfs -L root "$btrfsDev"
    mount -t btrfs "$btrfsDev" $MNT/
    btrfs subvolume create $MNT/nixos
    umount $MNT/
    mount -t btrfs -o subvol=nixos "$btrfsDev" $MNT/
    btrfs subvolume create $MNT/var
    btrfs subvolume create $MNT/home
    btrfs subvolume create $MNT/tmp
}

mountBoot() {
    mkdir -p "$MNT/boot"
    mount /dev/disk/by-label/boot "$MNT/boot"
}

################################################################################
##  prepare  ###################################################################
################################################################################

if [[ "$BOOTSTRAP" != "YES" ]]; then
    help
    exit 1
fi

if [ "$(id -u)" -ne 0 ]; then
    echo "you should run this script as root"
    exit 1
fi

SDX="$1"
MNT=${2:-/mnt}

if [[ ! -e "$SDX" ]]; then
    echo "$SDX not found"
    help
    exit 1
fi
mkdir -p "$MNT"
if [ "$(ls -A $MNT)" ]; then
    echo "$MNT not empty"
    help
    exit 1
fi

################################################################################
##  run  #######################################################################
################################################################################

set -x

if [[ -d /sys/firmware/efi/efivars ]]; then
    mkEfiPartitions
else
    mkLegacyPartitions
fi
SDX1="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 1p)"
SDX2="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 2p)"

formatBoot "$SDX2"
mkBTRFS "$SDX1"
mountBoot

doMounting

nixos-generate-config --root "$MNT"
