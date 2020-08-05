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
    local config=${1:-"$ROOT/configuration.nix"}
    cd "$nixpkgs/nixos"
    export NIX_PATH=nixpkgs="$nixpkgs":nixos-config="$config"
    set -x
    nix-build \
        --show-trace \
        --no-out-link \
        -I nixpkgs="$nixpkgs" \
        -I nixos-config="$config" \
        -A system
)

installBuiltSystem() {
    local system=$(buildSystem "$1")
    set -x
    sudo nixos-install --no-root-passwd --system "$system"
}

installBuiltSystem "$@"
