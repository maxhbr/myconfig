#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

# hosts from someonewhocares.org ##########################################
type "curl" &> /dev/null && {
    DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    [[ ! -f "$DIR/static/extrahosts" || "$(find "$DIR/static/extrahosts" -mtime +1)" != "" ]] && {
        echo "* $(tput bold)update hosts blacklist$(tput sgr0) ..."
        # curl http://someonewhocares.org/hosts/hosts | \
  #             #     sed -e '/<localhost>/,/<\/localhost>/d' > static/extrahosts
        # use hosts file from https://github.com/StevenBlack/hosts (MIT)
        curl https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts |
            grep ^0 > "$DIR/static/extrahosts"
    } || {
        echo "do not update hots file"
    }
}
