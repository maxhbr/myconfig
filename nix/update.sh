#!/usr/bin/env nix-shell
#! nix-shell -i bash -p curl gitMinimal git-lfs
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

. "$( dirname "${BASH_SOURCE[0]}" )/../common.sh"

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
            fi
        fi
    fi
}

update() {
    if [[ "$(cat /etc/nixos/hostname)" == "$my_main_host" ]]; then
        handleChannelAsSubtree "nixpkgs" "$nixStableChannel"
    fi

    nix_path_string="{ nix.nixPath = [\"nixpkgs=$nixpkgsDir\" \"nixpkgs-overlays=$overlaysDir\" \"nixos-config=$nixosConfigDir\"]; }"
    nix_path_file="/etc/nixos/imports/nixPath.nix"
    if [[ "$(cat $nix_path_file 2>/dev/null)" != *"$nix_path_string"* ]]; then
        sudo mkdir -p /etc/nixos/imports
        echo $nix_path_string |
            sudo tee $nix_path_file
    fi
}

update
