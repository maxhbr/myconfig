#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# TODO:
# - see also:
#   - https://github.com/ShikherVerma/i3lock-multimonitor/blob/master/lock
#   - 

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."

. $DIR/myBgs.sh

getScreenHeigth() {
    xrandr --current |
        grep '*' |
        uniq |
        awk '{print $1}' |
        cut -d 'x' -f2
}

lockWithRandom() {
    i3lock -t -i "$(getRandomBGFile $(getScreenHeigth))" -c 000000
}

lockWithRandom
