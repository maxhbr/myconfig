#!/usr/bin/env nix-shell
#! nix-shell -i bash -p parted btrfs-progs lvm2 cryptsetup util-linux
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see / based on:
# - https://nixos.org/nixos/manual/
# - https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134

set -e

BTRFS=${BTRFS:-true}
EFI=${EFI:-false}
EPHEMERAL_ROOT=${EPHEMERAL_ROOT:-false}

################################################################################
##  prepare  ###################################################################
################################################################################

help() {
    cat <<EOF
usage:
  $ sudo BOOTSTRAP=YES [BTRFS=true] [EFI=true] $0 \
      /dev/SDX \
      pass \
      [vg_name] \
      [mnt]
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
PASSPHRASE="$2"
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
##  functions  #################################################################
################################################################################

mkEfiPartitions() {
    parted -a optimal $SDX -- mklabel gpt
    parted -a optimal $SDX -- mkpart primary 81920MiB 100%
    parted -a optimal $SDX -- mkpart ESP fat32 1MiB 8192MiB
    parted -a optimal $SDX -- set 2 boot on
}

mkLegacyPartitions() {
    parted -a optimal $SDX -- mklabel msdos
    if true; then
        parted -a optimal $SDX -- mkpart primary 512MiB 100%
        parted -a optimal $SDX -- mkpart primary fat32 1MiB 512MiB
    else
        # swap as partition:
        parted -a optimal $SDX -- mkpart primary 512MiB -8GiB
        parted -a optimal $SDX -- mkpart primary fat32 1MiB 512MiB
        parted -a optimal $SDX -- mkpart primary linux-swap -8GiB 100%
    fi
    parted -a optimal $SDX -- set 2 boot on
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

mkBoot() {
    local bootDev="$1"

    mkfs.fat -F 32 -n boot "$bootDev"

    mkdir -p "$MNT/boot"

    sync; sleep 5
    mount /dev/disk/by-label/boot "$MNT/boot"
}

mkLVM() {
    local lvmDev="$1"

    pvcreate "$lvmDev"
    vgcreate "$VG_NAME" "$lvmDev"

    lvcreate -L 8G -n swap "$VG_NAME"
    lvcreate -l '100%FREE' -n root "$VG_NAME"

    sync; sleep 5

    mkSwap "/dev/$VG_NAME/swap"
    mkExt4 "/dev/$VG_NAME/root"
}

mkBTRFS() {
    # see: https://gist.github.com/samdroid-apps/3723d30953af5e1d68d4ad5327e624c0
    # see also for ephemeral root: https://gist.github.com/giuseppe998e/629774863b149521e2efa855f7042418
    local btrfsDev="$1"

    mkfs.btrfs -f -L root "$btrfsDev"

    # Create subvolumes
    mount -t btrfs "$btrfsDev" $MNT/
    if [[ "$EPHEMERAL_ROOT" == "true" ]]; then
        btrfs subvolume create $MNT/@persistent_priv
        btrfs subvolume create $MNT/@persistent_work
        btrfs subvolume create $MNT/@persistent_cache
    else
        btrfs subvolume create $MNT/@
        btrfs subvolume create $MNT/@snapshots
    fi
    btrfs subvolume create $MNT/@nix
    btrfs subvolume create $MNT/@home
    btrfs subvolume create $MNT/@log
    btrfs subvolume create $MNT/@swapfile
    umount $MNT/

    # Mount subvolumes
    if [[ "$EPHEMERAL_ROOT" == "true" ]]; then
        mount -t tmpfs -o noatime,mode=755 none $MNT
    else
        mount -t btrfs -o compress=zstd,subvol=@ "$btrfsDev" $MNT/
        mount -t btrfs -o compress=zstd,subvol=@snapshots "$btrfsDev" $MNT/.snapshots
    fi
    mkdir -p $MNT/{nix,var/log,.snapshots}
    mount -t btrfs -o compress=zstd,subvol=@nix "$btrfsDev" $MNT/nix
    mount -t btrfs -o compress=zstd,subvol=@log "$btrfsDev" $MNT/var/log

    mkdir -p $MNT/home
    mount -t btrfs -o compress=zstd,subvol=@home "$btrfsDev" $MNT/home

    # to exclude from snapshots:
    btrfs subvolume create $MNT/home/docker
    if [[ "$EPHEMERAL_ROOT" == "true" ]]; then
        mkdir -p $MNT/{persistent/priv,persistent/work,persistent/cache}
        mount -t btrfs -o compress=zstd,subvol=@persistent_priv "$btrfsDev" $MNT/persistent/priv
        mount -t btrfs -o compress=zstd,subvol=@persistent_work "$btrfsDev" $MNT/persistent/work
        mount -t btrfs -o compress=zstd,subvol=@persistent_cache "$btrfsDev" $MNT/persistent/cache
    fi

    mkdir -p $MNT/.swapfile
    mount -t btrfs -o compress=no,subvol=@swapfile "$btrfsDev" $MNT/.swapfile
    sudo touch $MNT/.swapfile/swapfile
    sudo chattr +C $MNT/.swapfile/swapfile
    sudo chmod 600 $MNT/.swapfile/swapfile
    sudo fallocate -l20g $MNT/.swapfile/swapfile
    sudo mkswap $MNT/.swapfile/swapfile
}

mkExt4() {
    local rootDev="$1"

    mkfs.ext4 -L root "$rootDev"

    sync; sleep 5
    mount /dev/disk/by-label/root "$MNT"
}

mkRoot() {
    local dev="$1"

    if [[ "$BTRFS" == "true" ]]; then
        mkBTRFS "$dev"
    else
        mkLVM "$dev"
    fi
}

################################################################################
##  run  #######################################################################
################################################################################

set -x

if vgdisplay -c  | cut -f 1 -d ":" | tr -d '[:space:]' | grep -q '^'"$VG_NAME"'$'; then
    echo "$VG_NAME already present on system"
    echo
    echo "to remove, do:"
    echo '$ sudo swapoff /dev/'$VG_NAME'/swap'
    echo '$ sudo lvchange -an /dev/'$VG_NAME'/*'
    echo '$ sudo lvremove /dev/'$VG_NAME'/*'
    echo '$ sudo vgremove '$VG_NAME
    echo '$ sudo cryptsetup close /dev/mapper/enc-pv'
else
    if [[ -d /sys/firmware/efi/efivars || "$EFI" == "true" ]]; then
        mkEfiPartitions
    else
        mkLegacyPartitions
    fi
    SDX1="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 1p)"
    SDX2="$(fdisk -l "$SDX" | grep '^/dev' | cut -d' ' -f1 | sed -n 2p)"

    if [[ -z "$PASSPHRASE" ]]; then
        mkRoot "$SDX1"
    else
        mkLuks "$SDX1"
        mkRoot /dev/mapper/enc-pv
    fi
    mkBoot "$SDX2"
fi

nixos-generate-config --root "$MNT"

cat <<EOF
create networking.hostId with:
$ cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
EOF
