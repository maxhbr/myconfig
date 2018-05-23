#!/usr/bin/env bash
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e
set -x

nixpkgsDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

gate() {
    nix-env --version &> /dev/null
}

updateForChannel() {
    channel=$1

    echo "... update for channel $channel"

    target="$nixpkgsDir/${channel}.json"

    rev=$(curl -L -s "https://nixos.org/channels/${channel}/git-revision")

    if [[ -f $target ]]; then
        if grep -q $rev $target; then
            echo "... rev=[$rev] is already fetched"
            return
        fi
    fi


    tarball="https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz"
    hash=$(nix-prefetch-url --type sha256 $tarball)
    bothLinesForUnpacked=$(nix-prefetch-url --unpack --print-path --type sha256 $tarball)
    hashForUnpacked=$(echo "$bothLinesForUnpacked" | head -1)
    pathForUnpacked=$(echo "$bothLinesForUnpacked" | tail -1)

    echo '{"url":"'$tarball'","rev": "'$rev'","sha256":"'$hash'","outputSha256":"'$hashForUnpacked'","path":"'$pathForUnpacked'"}' | tee $target
}

prepare() {
    echo "* $(tput bold)upgrade nixpks version$(tput sgr0) ..."
    updateForChannel nixos-18.03
    updateForChannel nixos-unstable
}

gate || {
    echo "... skip"
    exit 0
}
if [ $# -eq 0 ]; then
    prepare
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
