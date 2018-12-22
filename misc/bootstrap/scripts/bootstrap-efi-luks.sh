#!/usr/bin/env nix-shell
#! nix-shell -i bash -p git git-lfs
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see / based on:
# - https://nixos.org/nixos/manual/
# - https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134

set -ex

if [[ "$BOOTSTRAP" != "YES" ]]; then
    exit 1
fi

SDX="$1"
PASSPHRASE=${2:-pass}

# parted ${SDX} -- mklabel msdos
# parted ${SDX} -- mkpart primary 1MiB -8GiB
# parted ${SDX} -- mkpart primary linux-swap -8GiB 100%
parted $SDX -- mklabel gpt
parted $SDX -- mkpart primary 512MiB 100%
parted $SDX -- mkpart ESP fat32 1MiB 512MiB
parted $SDX -- set 2 boot on

SDX1="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 1p)"
SDX2="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 2p)"

echo -n "$PASSPHRASE" | cryptsetup --batch-mode luksFormat $SDX1 -
echo -n "$PASSPHRASE" | cryptsetup --batch-mode luksOpen $SDX1 enc-pv -

pvcreate /dev/mapper/enc-pv
vgcreate vg /dev/mapper/enc-pv
lvcreate -L 8G -n swap vg
lvcreate -l '100%FREE' -n root vg

mkfs.ext4 -L root /dev/vg/root
mkswap -L swap /dev/vg/swap
swapon /dev/vg/swap
mkfs.fat -F 32 -n boot $SDX2
mount /dev/disk/by-label/root /mnt
mkdir -p /mnt/boot
mount /dev/disk/by-label/boot /mnt/boot
nixos-generate-config --root /mnt

git clone https://github.com/maxhbr/myconfig /mnt/myconfig
