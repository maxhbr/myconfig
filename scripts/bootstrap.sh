#!/usr/bin/env nix-shell
#! nix-shell -i bash -p git
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# without encryption, for luks encryption follow https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134

exit 1 # <-- remove

SDX="/dev/sda"

# parted ${SDX} -- mklabel msdos
# parted ${SDX} -- mkpart primary 1MiB -8GiB
# parted ${SDX} -- mkpart primary linux-swap -8GiB 100%
parted ${SDX} -- mklabel gpt
parted ${SDX} -- mkpart primary 512MiB -8GiB
parted ${SDX} -- mkpart primary linux-swap -8GiB 100%
parted ${SDX} -- mkpart ESP fat32 1MiB 512MiB
parted ${SDX} -- set 3 boot on

mkfs.ext4 -L nixos ${SDX}1
mkswap -L swap ${SDX}2
swapon ${SDX}2
mkfs.fat -F 32 -n boot ${SDX}3        # (for UEFI systems only)
mount /dev/disk/by-label/nixos /mnt
mkdir -p /mnt/boot                      # (for UEFI systems only)
mount /dev/disk/by-label/boot /mnt/boot # (for UEFI systems only)
nixos-generate-config --root /mnt
nano /mnt/etc/nixos/configuration.nix
nixos-install

git clone https://github.com/maxhbr/myconfig /mnt/myconfig

reboot
