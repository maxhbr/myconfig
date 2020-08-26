#!/usr/bin/env nix-shell
#! nix-shell -i bash -p packer
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

ROOT="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
cd "$ROOT"

build_with_qemu() {
    echo "##########################################################################"
    echo "## Build"
    time packer build -on-error=ask -only=qemu packer.json
}

build_with_virtualbox() {
    echo "##########################################################################"
    echo "## Build"
    rm myconfig-virtualbox.box || true
    time packer build -on-error=ask -only=virtualbox-iso packer.json
    du -h myconfig-virtualbox.box

    echo "##########################################################################"
    echo "## To Vagrant"
    vagrant box remove myconfig || true
    time vagrant box add myconfig myconfig-virtualbox.box
    # time vagrant plugin install vagrant-nixos
}


case "$1" in
    qemu) build_with_qemu ;;
    vbox) build_with_virtualbox ;;
    *) cat <<EOF
usage:
\$ $0 qemu
\$ $0 vbox
EOF
       ;;
esac

times
