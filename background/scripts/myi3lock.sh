#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
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
    i3lock -i "$(getRandomBGFile $(getScreenHeigth))"
}

lockWithRandom
