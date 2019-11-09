#!/usr/bin/env bash
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

gate() {
    [ -d /etc/nixos ]
    nixos-version &> /dev/null
}

deploy() {
    if [[ ! -f /etc/nixos/hostid ]]; then
        echo "set hostid:"
        cksum /etc/machine-id |
            while read c rest; do printf "%x" $c; done |
            sudo tee /etc/nixos/hostid
    fi
}

upgrade() {
    [[ "$MYCONFIG_ARGS" == *"--fast"* ]] &&
        args="--fast"

    logH3 "nixos-rebuild" "$args"

    sudo \
         NIX_CURL_FLAGS='--retry=1000' \
         nixos-rebuild \
             $NIX_PATH_ARGS \
             --show-trace --keep-failed \
             --upgrade \
             -I nixos-config="$nixosConfigDir/default.nix" \
             $args \
             --fallback ${NIXOS_REBUILD_CMD:-switch}
}
gate || {
    echo "... skip"
    exit 0
}
if [ $# -eq 0 ]; then
    deploy
    upgrade
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
