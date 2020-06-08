#!/usr/bin/env nix-shell
#! nix-shell -i bash -p parted cryptsetup btrfsProgs utillinux

# see: https://gist.github.com/MaxXor/ba1665f47d56c24018a943bb114640d7

WORKDIR="$HOME/mkEncBtrfs.tmp"
KEYFILE="/etc/cryptkey"
MOUNTPOINT="/mnt/data"

set -e

mkdir -p "$WORKDIR"
exec &> >(tee -a "$WORKDIR/log")

mkCryptKeyfile() {
    >&2 echo "### generate Keyfile"

    (set -ex;
     sudo dd bs=64 count=1 if=/dev/urandom of=$KEYFILE iflag=fullblock
     sudo chmod 600 $KEYFILE
     sudo cp "$KEYFILE" "$WORKDIR"
    ) 1>&2
}

makePartition() {
    >&2 echo "### makePartition $@"
    SDX="$1"

    (set -ex;
     sudo parted -s $SDX -- mklabel gpt;
     sudo parted -s $SDX -- mkpart primary 1MiB 100%
    ) 1>&2

    echo "${SDX}1"
}

encryptPartition() {
    >&2 echo "### encryptPartition $@"
    local partition="$1"
    local name="data-$(basename "$partition")"

    (set -ex;
     sudo cryptsetup -q -y -v luksFormat "$partition" "$KEYFILE";
     sudo cryptsetup -q luksHeaderBackup --header-backup-file "$WORKDIR/$(basename "$partition").header.bak" "$partition";
     sudo cryptsetup -q open --key-file="$KEYFILE" --type luks "$partition" "$name"

     sudo blkid "$partition"
    ) 1>&2

    echo "/dev/mapper/$name"
}

calculateRaidLevel() {
    >&2 echo "### calculateRaidLevel $@"
    local nEncs="$1"

    if [[ "$nEncs" -ge "4" ]]; then
        if [ $((nEncs%2)) -eq 0 ]; then
            echo "RAID10"
        else
            echo "RAID5"
        fi
    else
        echo "RAID1"
    fi
}

partitionBtrfsRaid() {
    >&2 echo "### partitionBtrfsRaid $@"
    local encs=("$@")
    local nEncs="${#encs[@]}"
    local raidLevel="$(calculateRaidLevel "$nEncs")"

    (set -x;
     sudo mkfs.btrfs -f -m $raidLevel -d $raidLevel ${encs[*]}
    ) 1>&2
}

mountBtrfsRaid() {
    >&2 echo "### mountBtrfsRaid $@"
    local firstEnc="$1"

    (set -ex;
     sudo mkdir -p "$MOUNTPOINT";
     sudo mount -t btrfs -o defaults,noatime,compress=zstd "$firstEnc" "$MOUNTPOINT";
     sudo btrfs subvolume create "$MOUNTPOINT/@sub";
     sudo umount "$MOUNTPOINT";
     sudo mount -t btrfs -o defaults,noatime,compress=zstd,subvol=@sub "$firstEnc" "$MOUNTPOINT"
    ) 1>&2
}

main() {
    >&2 echo "### main $@"

    mkCryptKeyfile
    encs=()
    for drive in "$@"; do
        enc="$(encryptPartition "$(makePartition "$drive")")"
        encs+=( "$enc" )
    done
    partitionBtrfsRaid ${encs[*]}
    mountBtrfsRaid "${encs[0]}"
}

main $@
