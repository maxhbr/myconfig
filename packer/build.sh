#!/usr/bin/env nix-shell
#! nix-shell -i bash -p packer
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

ROOT="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
cd "$ROOT"

## Cleanup
rm myconfig_virtualbox.box || true
if [[ -d _vup ]]; then
    pushd _vup
    vagrant destroy -f || true
    popd
    rm -rf _vup
fi
vagrant box remove myconfig || true
rm myconfig.bundle || true

## Prepare
git bundle create myconfig.bundle --all

## Build
packer build packer.json

## To Vagrant
vagrant box add myconfig myconfig-virtualbox.box
vagrant plugin install vagrant-nixos

## Test Vagrant
mkdir -p _vup
cd _vup
vagrant init myconfig
vagrant up
