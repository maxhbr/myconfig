#!/usr/bin/env bash
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

build() (
    local nixpkgsDir="$nixpkgs"
    time nix-build \
         -A config.system.build.sdImage \
         --option system aarch64-linux \
         -I nixos-config="$myconfigDir/hosts/aarch64/pi3a/default.nix" \
         -I nixpkgs="$nixpkgsDir" \
         --no-out-link \
         --show-trace --keep-failed \
         --option sandbox false \
         "${nixpkgsDir}/nixos/default.nix"
)

set -x
drv=$(build)
out=("$drv/sd-image/"*)
du -h "$out"
install -D -v "$out" -t "$myconfigDir/__out/pi4"
set +x
