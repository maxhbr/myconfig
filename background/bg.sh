#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../background"
cd $DIR
bgs=(*.png) # bgs=("quint4.png" "penrose_4k_color.png" "quint3.png" "quint5.png" "quint7.png" "romben3.png" "romben.png")

getRandomBG() {
    rand=$[$RANDOM % ${#bgs[@]}]
    img="${bgs[$rand]}"

    if [ "$1" ]; then
        if [ -d "$DIR/$1" ]; then
            echo "$DIR/$1/${bgs[$rand]}"
            return
        fi
    fi
    echo "$DIR/${bgs[$rand]}"
}

setRandomWithFeh() {
    [ -z "$DISPLAY" ] && \
        export DISPLAY=:0
    feh --bg-scale "$(getRandomBG)"
}

getScreenHeigth() {
    xrandr --current |
        grep '*' |
        uniq |
        awk '{print $1}' |
        cut -d 'x' -f2
}

lockWithRandom() {
    echo "lock at: $(date)" | tee -a "/tmp/myScreenLock.sh.log"
    i3lock -t -i "$(getRandomBG $(getScreenHeigth))" -c 000000
}

case $1 in
    --set) setRandomWithFeh ;;
    --lock) lockWithRandom ;;
    *) getRandomBG ;;
esac
