#!/usr/bin/env nix-shell
#! nix-shell -i bash -p curl gitMinimal git-lfs
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../common.sh"

gate() {
    nix-env --version &> /dev/null
}

addRemotesIfNecessary() {
    cd $myconfigDir
    local remotes=$(git remote)
    if [[ "$remotes" != *'NixOS-nixpkgs-channels'* ]]; then
        git remote add NixOS-nixpkgs-channels https://github.com/NixOS/nixpkgs-channels
    fi
}

handleChannelAsSubtree() {
    local dir=nix/$1
    local channel=$2

    if [[ "$MYCONFIG_ARGS" == *"--fast"* ]]; then
        echo "skip handling $channel (in $dir)"
    else
        addRemotesIfNecessary

        logINFO "the channel $channel was last updated $(git log --format="%cr" remotes/NixOS-nixpkgs-channels/$channel -1)"

        cd $myconfigDir
        git fetch NixOS-nixpkgs-channels -- $channel
        if [ ! -f "$dir/default.nix" ]; then
            git subtree add --prefix $dir NixOS-nixpkgs-channels $channel --squash
        else
            if git diff-index --quiet HEAD --; then
                (set -x;
                 git subtree pull --prefix $dir NixOS-nixpkgs-channels $channel --squash
                )
            else
                logERR "uncommitted changes, do not update $channel"
                # logINFO "stash local changes to allow subtree pull"
                # git stash push -m "autostash for nix/default.sh"
                # git subtree pull --prefix $dir NixOS-nixpkgs-channels $channel --squash
                # git stash pop "stash@{0}" 1> /dev/null
            fi
        fi
    fi
}

prepare() {
    if [[ "$(cat /etc/nixos/hostname)" == "$my_main_host" ]]; then
        handleChannelAsSubtree "nixpkgs" "$nixStableChannel"
        $nixConfigDir/overlays/nixpkgs-unstable/default.sh
    fi

    nix_path_string="{ nix.nixPath = [\"nixpkgs=$nixpkgsDir\" \"nixpkgs-overlays=$overlaysDir\" \"nixos-config=$nixosConfigDir\"]; }"
    nix_path_file="/etc/nixos/imports/nixPath.nix"
    if [[ "$(cat $nix_path_file 2>/dev/null)" != *"$nix_path_string"* ]]; then
        sudo mkdir -p /etc/nixos/imports
        echo $nix_path_string |
            sudo tee $nix_path_file
    fi
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
    prepare
    deploy
    cleanup
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
