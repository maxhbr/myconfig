#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nixos-generators

set -e

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../../common.sh"

set -x

lxc-metadata() {
    nixos-generate $NIX_PATH_ARGS -f lxc-metadata |
        xargs -r cat |
        awk '{print $3}'
}

lxc() {
    nixos-generate $NIX_PATH_ARGS -c configuration.nix -f lxc |
        xargs -r cat |
        awk '{print $3}'
}

sudo lxc image import --alias myconfig "$(lxc-metadata)" "$(lxc)"
