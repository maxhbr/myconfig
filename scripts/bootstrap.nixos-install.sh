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

genConfig() {
    local cfg=$(mktemp /tmp/bootstrap.nixos-install.XXXXXXXX.nix)
    cat <<EOF > "$cfg"
{ pkgs, ... }:
{ imports =
    [ ${myconfigDir}/modules
      /mnt/etc/nixos/configuration.nix
    ];

  config = {
    networking.hostName = "bootstrapped";
    networking.hostId = "12345678";
  };
}
EOF

    echo $cfg
}

buildSystem() (
    local config=${1:-"$(genConfig)"}
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
