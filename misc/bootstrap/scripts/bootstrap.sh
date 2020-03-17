#!/usr/bin/env nix-shell
#! nix-shell -i bash -p parted lvm2 cryptsetup utillinux
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
  $ BOOTSTRAP=YES $0 /dev/SDX [pass] [vg_name] [mnt]
  $ BOOTSTRAP=YES $0 /dev/SDX "" [vg_name] [mnt]
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

mkLuks() {
    local luksDev="$1"

    echo -n "$PASSPHRASE" | cryptsetup --batch-mode luksFormat "$luksDev" -
    echo -n "$PASSPHRASE" | cryptsetup --batch-mode luksOpen "$luksDev" enc-pv -
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

mkLVM() {
    local lvmDev="$1"

    pvcreate "$lvmDev"
    vgcreate "$VG_NAME" "$lvmDev"

    lvcreate -L 8G -n swap "$VG_NAME"
    lvcreate -l '100%FREE' -n root "$VG_NAME"

    mkSwap "/dev/$VG_NAME/swap"
    formatRoot "/dev/$VG_NAME/root"
}

doMounting() {
    mount /dev/disk/by-label/root "$MNT"
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
PASSPHRASE=${2:-pass}
VG_NAME=${3:-vg}
MNT=${4:-/mnt}

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

if vgdisplay -c  | cut -f 1 -d ":" | tr -d '[:space:]' | grep -q '^'"$VG_NAME"'$'; then
    echo "$VG_NAME already present on system"
else
    if [[ -d /sys/firmware/efi/efivars ]]; then
        mkEfiPartitions
    else
        mkLegacyPartitions
    fi
    SDX1="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 1p)"
    SDX2="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 2p)"

    formatBoot "$SDX2"
    if [[ -z "$PASSPHRASE" ]]; then
        mkLVM "$SDX1"
    else
        mkLuks "$SDX1"
        mkLVM /dev/mapper/enc-pv
    fi
fi

doMounting

nixos-generate-config --root "$MNT"
