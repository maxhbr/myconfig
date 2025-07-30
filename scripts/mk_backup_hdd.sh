#!/usr/bin/env nix-shell
#! nix-shell -i bash -p parted btrfs-progs lvm2 cryptsetup util-linux
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see / based on:
# - https://nixos.org/nixos/manual/
# - https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134

set -euo pipefail

################################################################################
##  prepare  ###################################################################
################################################################################

help() {
    cat <<EOF
usage:
  $ FORMAT=YES $0 \
      /dev/SDX \
      pass
EOF
}

if [[ "$FORMAT" != "YES" || "$#" -ne 2 ]]; then
    help
    exit 1
fi

if [ "$(id -u)" -eq 0 ]; then
    echo "you should not run this script as root"
    exit 1
fi


SDX="$1"
PASSPHRASE="$2"

if [[ -z "$SDX" || -z "$PASSPHRASE" ]]; then
    echo "missing arguments"
    help
    exit 1
fi

if [[ ! -e "$SDX" ]]; then
    echo "$SDX not found"
    help
    exit 1
fi

################################################################################
##  functions  #################################################################
################################################################################

mkPartitions() {
    local SDX="$1"
    echo "###########################################################################################"
    echo "mkPartitions $SDX"
    echo "###########################################################################################"

    sudo parted -a optimal $SDX -- mklabel gpt
    # create two partitions, first 100GB for ext4 and the rest for luks with btrfs
    sudo parted -a optimal $SDX -- mkpart primary 1MiB 100GiB
    sudo parted -a optimal $SDX -- mkpart primary 100GiB 100%
}

mkExt4() {
    local ext4Dev="$1"
    local name="$2"

    echo "###########################################################################################"
    echo "mkExt4 $ext4Dev $name"
    echo "###########################################################################################"

    sudo mkfs.ext4 -L "$name" "$ext4Dev"
    sync; sleep 5
}

mkLuks() {
    local luksDev="$1"
    local name="$2"

    echo "###########################################################################################"
    echo "mkLuks $luksDev $name"
    echo "###########################################################################################"

    echo -n "$PASSPHRASE" | sudo cryptsetup --batch-mode luksFormat "$luksDev" -
    echo -n "$PASSPHRASE" | sudo cryptsetup --batch-mode luksOpen "$luksDev" "$name" -

    sleep 3
    sync
    local uuid=$(set -x; lsblk -nr -o UUID,NAME | rg "$(basename "$luksDev")" | cut -f 1 -d " "  )
    while [[ -z "$uuid" ]]; do
        echo "UUID of $dev: $uuid" >&2
        sleep 1
        uuid=$(set -x; lsblk -nr -o UUID,NAME | rg "$(basename "$luksDev")" | cut -f 1 -d " "  )
    done

    echo "###########################################################################################"
    local passName="hardware/backup/${name}_${uuid}"
    echo "insert into pass at $passName"
    echo "###########################################################################################"


    # add passphrase to pass .password-store:
    echo "$PASSPHRASE" | pass insert --multiline --force "$passName"

    echo "###########################################################################################"
    local hddKeyFile="$HOME/.password-store/$passName.key"
    echo "add keyFile to luks at $hddKeyFile"
    echo "###########################################################################################"

    dd if=/dev/random of="$hddKeyFile" bs=4096 count=1
    echo -n "$PASSPHRASE" | sudo cryptsetup luksAddKey "$luksDev" "$hddKeyFile"
}


mkBTRFS() {
    local btrfsDev="$1"
    local name="$2"

    echo "###########################################################################################"
    echo "mkBTRFS $btrfsDev $name"
    echo "###########################################################################################"

    sudo mkfs.btrfs -f -L "$name" "$btrfsDev"
}


################################################################################
##  run  #######################################################################
################################################################################

sudo cryptsetup luksClose backupLuks || true

mkPartitions "$SDX"
SDX1="$(sudo fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 1p)"
SDX2="$(sudo fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 2p)"
if [[ -z "$SDX1" || -z "$SDX2" ]]; then
    echo "failed to get partitions"
    exit 1
fi

mkExt4 "$SDX1" "backupUnenc"
mkLuks "$SDX2" "backupLuks"
mkBTRFS "/dev/mapper/backupLuks" "backupEnc"
