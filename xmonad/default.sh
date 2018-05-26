#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

deploy() {
    if nixos-version > /dev/null; then
        return
    fi

    echo "* $(tput bold)deploy xmonad$(tput sgr0) ..."

    local xmonadSrc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    local target="$HOME/.xmonad"

    # deploy ##################################################################
    mkdir -p "$target"
    declare -a files=(lib bin share xmonad.hs xmobarrc)
    for file in ${files[@]}; do
        [[ -e "$target/$file" ]] || ln -s "$xmonadSrc/$file" "$target/$file"
    done

    # notification_pipe="$target/notification.pipe"
    # [[ -p $notification_pipe ]] || mkfifo $notification_pipe
}

upgrade() {
    if nixos-version > /dev/null; then
        return
    fi

    echo "* $(tput bold)recompile and restart xmonad$(tput sgr0) ..."
    if [ ! -z ${DISPLAY+x} ]; then
        if nixos-version > /dev/null; then
            xmonad --restart
        else
            xmonad --recompile
            sleep 0.1
            xmonad --restart
        fi
    fi
}

if [ $# -eq 0 ]; then
    deploy
    upgrade
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
