#!/usr/bin/env nix-shell
#! nix-shell -i bash -p curl
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

gate() {
    nix-env --version &> /dev/null
}

gate || {
    echo "... skip"
    exit 0
}

buildNixCmd() {
    local nixEnvCMD="nix-env"
    if [[ -d $nixpkgsUnstableDir ]]; then
        nixEnvCMD="$nixEnvCMD -I nixpkgs=$nixpkgsUnstableDir"
    else
        nixEnvCMD="$nixEnvCMD -I nixpkgs=$nixpkgsDir"
    fi
    nixEnvCMD="$nixEnvCMD -I nixpkgs-overlays=$overlaysDir"
    nixEnvCMD="$nixEnvCMD -I nixos-config=$nixosConfigDir"

    echo $nixEnvCMD
}

prepareDirs() {
    cd $myconfigDir
    remotes=$(git remote)
    if [[ "$remotes" != *'NixOS-nixpkgs'* ]]; then
        git remote add NixOS-nixpkgs https://github.com/NixOS/nixpkgs
    fi
    git fetch NixOS-nixpkgs
    if [[ ! -f "$nixpkgsDir/.git" ]]; then
        git worktree add "$nixpkgsDir" HEAD
    fi
    if [[ ! -f "$nixpkgsUnstableDir/.git" ]]; then
        git worktree add "$nixpkgsUnstableDir" HEAD
    fi
}

getLatestRevForChannel() {
    local channel=$1
    curl -L -s "https://nixos.org/channels/${channel}/git-revision"
}

updateDirForRev() {
    local dir=$1
    local rev=$2

    (
        cd $dir
        if [[ $(git log --pretty=%H | head -1) != *"$rev"* ]]; then
            echo "... set dir=[$dir] to rev=[$rev]"
            git checkout $rev
        else
            echo "... dir=[$dir] is already at rev=[$rev]"
        fi
    )
}

updateDirForChannel() {
    local dir=$1
    local channel=$2

    local latestRev=$(getLatestRevForChannel $channel | tee "$nixConfigDir/$channel.rev")
    updateDirForRev $dir $latestRev
}

updateDirForRevByChannel() {
    local dir=$1
    local channel=$2

    local currentRev=$(cat "$nixConfigDir/$channel.rev")
    updateDirForRev $dir $currentRev
}

handleChannel() {
    local dir=$1
    local channel=$2

    if [[ "$MYCONFIG_ARGS" == *"--fast"* ]]; then
        echo "* $(tput bold)setup for channel=[$channel]$(tput sgr0) ..."
        updateDirForRevByChannel $dir $channel
    else
        echo "* $(tput bold)upgrade for channel=[$channel]$(tput sgr0) ..."
        updateDirForChannel $dir $channel
    fi
}

prepare() {
    prepareDirs

    (cd "$nixpkgsDir"; git fetch origin)

    handleChannel "$nixpkgsDir" nixos-18.03
    handleChannel "$nixpkgsUnstableDir" nixos-unstable

    echo "[\"nixpkgs=$nixpkgsDir\" \"nixpkgs-overlays=$overlaysDir\" \"nixos-config=$nixosConfigDir\"]" > "$nixConfigDir/nixPath.nix"
}

deploy() {
    configTarget=/etc/nix/nixpkgs-config.nix
    echo "* $(tput bold)update $configTarget$(tput sgr0) ..."
    if ! cmp "$nixConfigDir/nixpkgs-config.nix" $configTarget >/dev/null 2>&1; then
        sudo mkdir -p /etc/nix
        sudo cp "$nixConfigDir/nixpkgs-config.nix" $configTarget
    fi
}

upgrade() {
    echo "* $(tput bold)nix-env --upgrade$(tput sgr0) ..."
    NIX_CURL_FLAGS='--retry=1000' \
                  $(buildNixCmd) \
                  --show-trace \
                  --upgrade
}

cleanup() {
    if [ "$((RANDOM%100))" -gt 90 ]; then
        echo "* nix-env --delete-generations 30d ..."
        $(buildNixCmd) \
            --delete-generations 30d
        sudo $(buildNixCmd) \
            --delete-generations 30d
    else
        echo "* $(tput bold)do not$(tput sgr0) nix-env --delete-generations 30d ..."
    fi
}

if [ $# -eq 0 ]; then
    prepare
    upgrade
    cleanup
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
