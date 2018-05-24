#!/usr/bin/env nix-shell
#! nix-shell -i bash -p curl jq
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

nixpkgsDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

gate() {
    nix-env --version &> /dev/null
}

updateForChannel() {
    channel=$1

    echo "** update for channel $channel"

    target="$nixpkgsDir/${channel}.json"

    rev=$(curl -L -s "https://nixos.org/channels/${channel}/git-revision")

    if [[ -f $target ]]; then
        oldPath=$(cat $target | jq -r .path)
        oldRev=$(cat $target | jq -r .rev)
        if [ "$rev" == "$oldRev" ] && [ -d "$oldPath" ]; then
            echo "... rev=[$rev] is already fetched"
            return
        fi
    fi

    tarball="https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz"
    prefetchOutput=$(nix-prefetch-url --unpack --print-path --type sha256 $tarball)
    hash=$(echo "$prefetchOutput" | head -1)
    path=$(echo "$prefetchOutput" | tail -1)

    echo '{"url":"'$tarball'","rev": "'$rev'","sha256":"'$hash'","path":"'$path'"}' | tee $target
}

setupNixosFolder() {
    channel=$1

    echo "** setup nixos folder for channel $channel"

    json="$nixpkgsDir/${channel}.json"
    target="$nixpkgsDir/nixos"

    if [[ ! -f $json ]]; then
        echo "WARN: can not setup nixos folder, $json does not exist"
        return
    fi

    path=$(cat $json | jq -r .path)

    string='My long string'
    if [[ $(readlink -f $target) = *"$path"* ]]; then
        echo "... already up to date"
        return
    fi

    if [[ ! -d $path ]]; then
        echo "WARN: not yet prefetched, $path does not exist"
        return
    fi

    ln -sfT "$path/nixos" $target
}

prepare() {
    echo "* $(tput bold)upgrade nixpks version$(tput sgr0) ..."
    updateForChannel nixos-18.03
    updateForChannel nixos-unstable
}

deploy() {
    setupNixosFolder nixos-18.03
}

gate || {
    echo "... skip"
    exit 0
}

if [ $# -eq 0 ]; then
    prepare
    deploy
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
