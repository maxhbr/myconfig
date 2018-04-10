#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

echo "* $(tput bold)deploy xmonad$(tput sgr0) ..."

deploy() {
    xmonadSrc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    target="$HOME/.xmonad"

    # deploy ##################################################################
    mkdir -p "$target"
    declare -a files=(lib bin neo xmonad.hs xmobarrc)
    for file in ${files[@]}; do
        [[ -e "$target/$file" ]] || ln -s "$xmonadSrc/$file" "$target/$file"
    done

    # notification_pipe="$target/notification.pipe"
    # [[ -p $notification_pipe ]] || mkfifo $notification_pipe
}

upgrade() {
    echo "* $(tput bold)recompile and restart xmonad$(tput sgr0) ..."
    if [ ! -z ${DISPLAY+x} ]; then
        xmonad --recompile
        sleep 0.1
        xmonad --restart
    fi
}

([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
$@
