#!/usr/bin/env nix-shell
#! nix-shell -i bash -p curl gitMinimal git-lfs
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../common.sh"

gate() {
    nix-env --version &> /dev/null
}

deploy() {
    configTarget=/etc/nix/nixpkgs-config.nix
    if ! cmp "$nixConfigDir/nixpkgs-config.nix" $configTarget >/dev/null 2>&1; then
        echo "* $(tput bold)update $configTarget$(tput sgr0) ..."
        sudo mkdir -p /etc/nix
        sudo cp "$nixConfigDir/nixpkgs-config.nix" $configTarget
    fi
}

cleanup() {
    if [ "$((RANDOM%100))" -gt 90 ]; then
        echo "* nix-env --delete-generations 30d ..."
        nix-env $NIX_PATH_ARGS \
            --delete-generations 30d
        sudo nix-env $NIX_PATH_ARGS \
            --delete-generations 30d
    else
        echo "* $(tput bold)do not$(tput sgr0) nix-env --delete-generations 30d ..."
    fi
}


gate || {
    echo "... skip"
    exit 0
}
if [ $# -eq 0 ]; then
    deploy
    cleanup
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
