#!/usr/bin/env bash
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

cd "$( dirname "${BASH_SOURCE[0]}" )"

out=extrahosts

type "curl" &> /dev/null && {
    [[ ! -f "$out" || "$(find "$out" -mtime +1)" != "" ]] && {
        echo "* $(tput bold)update hosts blacklist$(tput sgr0) ..."
        # use hosts file from https://github.com/StevenBlack/hosts (MIT)
        curl https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts |
            grep ^0 > "$out"
        echo "0.0.0.0 navigationshilfe1.t-online.de" >> "$out"
    } || echo "do not update hots file"
}

