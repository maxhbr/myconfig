#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
LINK="$HOME/.background-image"

. $DIR/myBgs.sh

linkRandomToHome() {
    cd $(dirname $0)
    rm $LINK
    ln -s $(getRandomBGFile) $LINK
    echo "$LINK"
}

setRandomWithFeh() {
    feh --bg-scale "$(linkRandomToHome)"
}

setRandomWithFeh
