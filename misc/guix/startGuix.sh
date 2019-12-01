#!/usr/bin/env nix-shell
#! nix-shell -i bash -p qemu xz

# see: http://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html

# mount myconfig:
# add to fstab:
#    host0   /wherever    9p      trans=virtio,version=9p2000.L   0 0
# see: https://superuser.com/questions/628169/how-to-share-a-directory-with-the-host-without-networking-in-qemu

set -e
set -o pipefail

BASENAME="guix-system-vm-image-1.0.1.x86_64-linux"
URL="https://ftp.gnu.org/gnu/guix/$BASENAME.xz"
DIR="$HOME/Downloads/tmp"
IMG="$DIR/$BASENAME"

mkdir -p "$DIR"
set -x
if [[ ! -f "$IMG" ]]; then
    cd "$DIR"
    wget "$URL"
    unxz "$BASENAME.xz"
fi

qemu-system-x86_64 \
   -net user -net nic,model=virtio \
   -enable-kvm -m 1024 \
   -device virtio-blk,drive=myhd \
   -drive if=none,file="$IMG",id=myhd \
   -device virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5 \
   -chardev spicevmc,name=vdagent,id=vdagent \
   -device virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent,name=com.redhat.spice.0
