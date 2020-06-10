#!/usr/bin/env bash
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"
set -e
ROOT="$(pwd)"
cd "$nixpkgs/nixos"

if [[ ! -d "/mnt/etc/nixos/" ]]; then
    echo "folder /mnt/etc/nixos/ is missing"
    exit 1
fi

buildSystem() (
    set -x
    cd "$nixpkgs/nixos"
    nix-build \
        --show-trace \
        --no-out-link \
        -I nixpkgs "$nixpkgs" \
        -I nixos-config "$ROOT/configuration.nix" \
        -A system
)

set -x
sudo nixos-install --no-root-passwd --system $(buildSystem)
