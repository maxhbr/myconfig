#!/usr/bin/env nix-shell
#! nix-shell -i bash -p parted cryptsetup btrfsProgs util-linux

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
     if [[ ! -f "$KEYFILE" ]]; then
         sudo dd bs=64 count=1 if=/dev/urandom of=$KEYFILE iflag=fullblock
     fi
     sudo chmod 600 $KEYFILE
     sudo cp "$KEYFILE" "$WORKDIR"
    ) 1>&2
}

makePartition() {
    >&2 echo "### makePartition $@"
    SDX="$1"

    (set -ex;
     sudo parted -s $SDX -- mklabel gpt
     sudo parted -s $SDX -- mkpart primary 1MiB 100%
    ) 1>&2

    echo "${SDX}1"
}

encryptPartition() {
    >&2 echo "### encryptPartition $@"
    local partition="$1"
    local name="$2"
    local mapper="/dev/mapper/$name"

    (set -ex
     sudo cryptsetup -q -y -v luksFormat "$partition" "$KEYFILE"
     sudo cryptsetup -q luksHeaderBackup --header-backup-file "$WORKDIR/$(basename "$partition").header.bak" "$partition"
     sudo cryptsetup -q open --key-file="$KEYFILE" --type luks "$partition" "$name"

     [[ -e "$mapper" ]]

     echo "$name UUID=$(sudo blkid -o value -s UUID "$partition") $KEYFILE luks,noearly" | tee "$WORKDIR/crypttab"
    ) 1>&2

    echo "$mapper"
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

    (set -ex
     sudo mkfs.btrfs -f -m $raidLevel -d $raidLevel ${encs[*]}
    ) 1>&2
}

mountBtrfsRaid() {
    >&2 echo "### mountBtrfsRaid $@"
    local firstEnc="$1"

    (set -ex
     sudo mkdir -p "$MOUNTPOINT"
     sudo mount -t btrfs -o defaults,noatime,compress=zstd "$firstEnc" "$MOUNTPOINT"
     sudo btrfs subvolume create "$MOUNTPOINT/@sub"
     sudo umount "$MOUNTPOINT"
     sudo mount -t btrfs -o defaults,noatime,compress=zstd,subvol=@sub "$firstEnc" "$MOUNTPOINT"
    ) 1>&2
}

getName() {
    >&2 echo "### getName $@"
    local i="$1"

    echo "data${i}"
}

main() {
    >&2 echo "### main $@"

    mkCryptKeyfile
    local i=0
    local encs=()
    for drive in "$@"; do
        enc="$(encryptPartition "$(makePartition "$drive")" "$(getName $i)")"
        encs+=( "$enc" )
        ((i=i+1))
    done
    partitionBtrfsRaid ${encs[*]}
    mountBtrfsRaid "${encs[0]}"
}

main $@
