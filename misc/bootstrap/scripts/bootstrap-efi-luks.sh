#!/usr/bin/env nix-shell
#! nix-shell -i bash -p parted lvm2 cryptsetup utillinux
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see / based on:
# - https://nixos.org/nixos/manual/
# - https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134

set -e

help() {
    cat <<EOF
usage:
  $ BOOTSTRAP=YES $0 /dev/SDX [pass] [vg_name] [mnt]
EOF
}

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
if vgdisplay -c  | cut -f 1 -d ":" | tr -d '[:space:]' | grep -q '^'"$VG_NAME"'$'; then
    echo "$VG_NAME already present on system"
    help
    exit 1
fi
mkdir -p "$MNT"
if [ "$(ls -A $MNT)" ]; then
    echo "$MNT not empty"
    help
    exit 1
fi

set -x

################################################################################
# do the partitioning

# parted ${SDX} -- mklabel msdos
# parted ${SDX} -- mkpart primary 1MiB -8GiB
# parted ${SDX} -- mkpart primary linux-swap -8GiB 100%
parted $SDX -- mklabel gpt
parted $SDX -- mkpart primary 512MiB 100%
parted $SDX -- mkpart ESP fat32 1MiB 512MiB
parted $SDX -- set 2 boot on

SDX1="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 1p)"
SDX2="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 2p)"

################################################################################
# create encrypted volume with luks

echo -n "$PASSPHRASE" | cryptsetup --batch-mode luksFormat $SDX1 -
echo -n "$PASSPHRASE" | cryptsetup --batch-mode luksOpen $SDX1 enc-pv -

################################################################################
# create the LVM

pvcreate /dev/mapper/enc-pv
vgcreate "$VG_NAME" /dev/mapper/enc-pv
lvcreate -L 8G -n swap "$VG_NAME"
lvcreate -l '100%FREE' -n root "$VG_NAME"

################################################################################
# formatting and mounting

mkfs.ext4 -L root "/dev/$VG_NAME/root"
mkswap -L swap "/dev/$VG_NAME/swap"
swapon "/dev/$VG_NAME/swap"
mkfs.fat -F 32 -n boot $SDX2
mount /dev/disk/by-label/root "$MNT"
mkdir -p "$MNT/boot"
mount /dev/disk/by-label/boot "$MNT/boot"

################################################################################
# bootstrap configuration

nixos-generate-config --root "$MNT"
