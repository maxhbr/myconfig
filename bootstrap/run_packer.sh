#!/usr/bin/env nix-shell
#! nix-shell -i bash -p packer
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

ROOT="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
cd "$ROOT"

echo "##########################################################################"
echo "## Build"
rm myconfig-virtualbox.box || true
time packer build packer.json -on-error=ask -only=virtualbox-iso
du -h myconfig-virtualbox.box

echo "##########################################################################"
echo "## To Vagrant"
vagrant box remove myconfig || true
time vagrant box add myconfig myconfig-virtualbox.box
# time vagrant plugin install vagrant-nixos
