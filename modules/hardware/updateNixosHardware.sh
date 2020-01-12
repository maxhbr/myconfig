#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gitMinimal
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

thisdir="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
. "$thisdir/../../common.sh"

addRemotesIfNecessary() {
    local remotes=$(git remote)
    if [[ "$remotes" != *'NixOS-nixos-hardware'* ]]; then
        git remote add NixOS-nixos-hardware https://github.com/NixOS/nixos-hardware
    fi
}

updateNixosHardware() {
    git fetch NixOS-nixos-hardware master
    git subtree pull --prefix" $(realpath --relative-to="$myconfigDir" "$thisdir")" NixOS-nixos-hardware master --squash
}

cd $myconfigDir
if git diff-index --quiet HEAD --; then
    addRemotesIfNecessary
    updateNixosHardware
else
    logERR "uncommitted changes, do not update nixos-hardware"
fi

