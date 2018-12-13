#!/usr/bin/env nix-shell
#! nix-shell -i bash -p packer
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

ROOT="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
cd "$ROOT"

echo "##########################################################################"
echo "## Cleanup"
rm myconfig_virtualbox.box || true
if [[ -d _vup ]]; then
    pushd _vup
    vagrant destroy -f || true
    popd
    rm -rf _vup
fi
vagrant box remove myconfig || true

echo "##########################################################################"
echo "## Build"
time packer build packer.json
du -h myconfig-virtualbox.box

echo "##########################################################################"
echo "## To Vagrant"
time vagrant box add myconfig myconfig-virtualbox.box
sleep 10
time vagrant plugin install vagrant-nixos
sleep 10

echo "##########################################################################"
echo "## Test Vagrant"
mkdir -p _vup
cd _vup
vagrant init myconfig
sleep 10
vagrant up
vagrant ssh
