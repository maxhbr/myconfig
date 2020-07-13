#!/usr/bin/env bash
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

set -x
drv=$(nix-build \
    -A config.system.build.sdImage \
    --option system aarch64-linux \
    -I nixos-config="$myconfigDir/hosts/aarch64/pi4/default.nix" \
    -I nixpkgs="$nixpkgs" \
    --no-out-link \
    --show-trace --keep-failed \
    "$nixpkgs/nixos/default.nix")
# --option sandbox false \
    #     chmod u+w result/sd-image/* && sudo cp result/sd-image/* /myconfig"]
out=("$drv/sd-image/"*)
du -h "$out"
mkdir -p "$HOME/Downloads/Images"
cp "$out" "$HOME/Downloads/Images"

set +x
times
