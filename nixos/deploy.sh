#!/usr/bin/env bash
# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

# hosts from someonewhocares.org ##########################################
type "curl" &> /dev/null && {
    [[ ! -f static/extrahosts || "$(find static/extrahosts -mtime +1)" != "" ]] && {
        echo "* $(tput bold)update hosts blacklist$(tput sgr0) ..."
        # curl http://someonewhocares.org/hosts/hosts | \
        #     sed -e '/<localhost>/,/<\/localhost>/d' > static/extrahosts
        # use hosts file from https://github.com/StevenBlack/hosts (MIT)
        curl https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts |
            grep ^0 > static/extrahosts
    }
}

set -e

# rsync file to target folder #############################################
echo "* $(tput bold)rsync$(tput sgr0) ..."
sudo rsync --filter="protect /hardware-configuration.nix" \
           --filter="protect /hostname" \
           --filter="protect /hostid" \
           --filter="exclude,s *.gitignore" \
           --filter="exclude,s *.gitmodules" \
           --filter="exclude,s *.git" \
           --filter="exclude .*.swp" \
           --filter="exclude .#*" \
           --filter="exclude result" \
           --delete --recursive --perms --copy-links \
           "$SRC/" /etc/nixos/
