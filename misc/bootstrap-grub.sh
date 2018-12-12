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

parted ${SDX} -- mklabel msdos
parted ${SDX} -- mkpart primary 1MiB -8GiB
parted ${SDX} -- mkpart primary linux-swap -8GiB 100%

SDX1="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 1p)"
SDX2="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 2p)"

mkfs.ext4 -L root $SDX1
mkswap -L swap $SDX2
swapon $SDX3
mount $SDX1 /mnt
nixos-generate-config --root /mnt

git clone https://github.com/maxhbr/myconfig /mnt/myconfig
