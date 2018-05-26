#!/usr/bin/env bash
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

gate() {
    [ -d /etc/nixos ]
    nixos-version &> /dev/null
}

prepare() {
    type "curl" &> /dev/null && {
        [[ ! -f "$nixosConfigDir/static/extrahosts" || "$(find "$nixosConfigDir/static/extrahosts" -mtime +1)" != "" ]] && {
            echo "* $(tput bold)update hosts blacklist$(tput sgr0) ..."
            # use hosts file from https://github.com/StevenBlack/hosts (MIT)
            curl https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts |
                grep ^0 > "$nixosConfigDir/static/extrahosts"
            echo "0.0.0.0 navigationshilfe1.t-online.de" >> "$nixosConfigDir/static/extrahosts"
        } || echo "do not update hots file"
    }
}

deploy() {
    logH3 "generate" "$configTarget"
    sudo mkdir -p /etc/nixos
    local configTarget=/etc/nixos/configuration.nix
    if [[ ! -f /etc/nixos/hostid ]]; then
        echo "set hostid:"
        cksum /etc/machine-id |
            while read c rest; do printf "%x" $c; done |
            sudo tee /etc/nixos/hostid
    fi
    if [[ ! -f $configTarget ]] || [[ $(wc -l <$configTarget) -eq 1 ]]; then
        line="let mypkgs = import <nixpkgs> {}; in mypkgs.myconfig.nixos-config"
        if ! grep -q "$line" "$configTarget"; then
            echo "$line" |
                sudo tee $configTarget
        fi
    else
        echo "$configTarget contains unexpected content"
        exit 1
    fi
}

upgrade() {
    [[ "$MYCONFIG_ARGS" == *"--fast"* ]] &&
        args="--fast"

    logH3 "nixos-rebuild" "$args"

    echo "... DEBUG: NIX_PATH=$NIX_PATH"
    sudo \
         NIX_CURL_FLAGS='--retry=1000' \
         nixos-rebuild \
             --show-trace --keep-failed \
             -I myconfigPath=$myconfigDir \
             -I nixpkgs=$nixpkgsDir \
             -I nixos-config=$nixosConfigDir \
             --upgrade \
             $args \
             --fallback ${1:-switch}
    echo "new generation: $(sudo nix-env -p /nix/var/nix/profiles/system --list-generations | head -1)"
}

cleanup() {
    type "nix-collect-garbage" &> /dev/null && {
        if [ "$((RANDOM%100))" -gt 90 ]; then
            echo "* nix-collect-garbage --delete-generations 30d ..."
            nix-collect-garbage --delete-older-than 30d
            sudo nix-collect-garbage --delete-older-than 30d
            echo "** kept generations are:"
            sudo nix-env -p /nix/var/nix/profiles/system --list-generations
            echo "** number of generations in the boot loader: $(ls /boot/loader/entries | wc -l)"
        else
            echo "* $(tput bold)do not$(tput sgr0) nix-collect-garbage --delete-generations 30d"
        fi
    }
}

gate || {
    echo "... skip"
    exit 0
}
if [ $# -eq 0 ]; then
    prepare
    deploy
    upgrade
    cleanup
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
