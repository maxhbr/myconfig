#!/usr/bin/env bash

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../../../common.sh"
set -e
cd "$nixpkgs/nixos"

if [[ ! -d "/mnt/etc/nixos/" ]]; then
    echo "folder /mnt/etc/nixos/ is missing"
    exit 1
fi

set -x
systemClosure=$(nix-build \
                    --show-trace \
                    --no-out-link \
                    $NIX_PATH_ARGS -A system)

sudo nixos-install --no-root-passwd --system $systemClosure
