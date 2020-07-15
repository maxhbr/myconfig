#!/usr/bin/env bash
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

build() (
    local hostname="$1"
    local nixpkgsDir="$nixpkgs"
    set -x
    time nix-build \
         -A config.system.build.sdImage \
         --option system aarch64-linux \
         -I nixos-config="${myconfigDir}/hosts/${hostname}/default.nix" \
         -I nixpkgs="$nixpkgsDir" \
         --no-out-link \
         --show-trace --keep-failed \
         --option sandbox false \
         "${nixpkgsDir}/nixos/default.nix"
)

buildAndCopy() {
    local hostname="$1"
    drv=$(build "$hostname")
    out=("$drv/sd-image/"*)
    du -h "$out"
    install -D -v "$out" -t "$myconfigDir/__out/$hostname"
    nix-store --delete "$drv"
}

buildAndCopy "$1"
