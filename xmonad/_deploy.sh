#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

echo "* $(tput bold)xmonad$(tput sgr0) ..."

src="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
target="$HOME/.xmonad"

# deploy ##################################################################
mkdir -p "$target"
declare -a files=(lib bin neo xmonad.hs xmobarrc)
for file in ${files[@]}; do
    [[ -e "$target/$file" ]] || ln -s "$src/$file" "$target/$file"
done

# rebuild and restart #####################################################
if [ ! -z ${DISPLAY+x} ]; then
    xmonad --recompile
    sleep 0.1
    xmonad --restart
fi

# notification_pipe="$target/notification.pipe"
# [[ -p $notification_pipe ]] || mkfifo $notification_pipe
