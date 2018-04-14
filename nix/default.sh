#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

echo "* $(tput bold)deploy nix configuration$(tput sgr0) ..."

gate() {
    type "nix-env" &> /dev/null
}

deploy() {
    sudo mkdir -p /etc/nix
    nixSrcDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    sudo ln -sfn "$nixSrcDir/pkgs" /etc/nix/pkgs
    sudo ln -sfn "$nixSrcDir/overlays" /etc/nix/overlays

    configTarget=/etc/nix/nixpkgs-configTarget.nix
    echo "* $(tput bold)generate $configTarget$(tput sgr0) ..."
    if [[ ! -f $configTarget ]] || [[ $(wc -l <$configTarget) -eq 1 ]]; then
        echo "import $nixSrcDir/nixpkgs-configTarget.nix" | sudo tee $configTarget
    else
        echo "$configTarget contains unexpected content"
        exit 1
    fi
}

upgrade() {
    echo "* $(tput bold)nix-env --upgrade$(tput sgr0) ..."
    NIX_PATH=
    NIX_CURL_FLAGS='--retry=1000' \
                  nix-env -I nixpkgs=channel:nixos-unstable \
                  -I nixpkgs-overlays=/etc/nix/overlays \
                  --show-trace \
                  --upgrade
}

cleanup() {
    if [ "$((RANDOM%100))" -gt 90 ]; then
        echo "* nix-env --delete-generations 30d ..."
        nix-env --delete-generations 30d
        sudo nix-env --delete-generations 30d
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
    upgrade
    cleanup
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
