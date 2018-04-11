#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."

[ -z "$DISPLAY" ] && \
    export DISPLAY=:0

setRandomWithFeh() {
    feh --bg-scale "$($DIR/getRandomBG.sh)"
}

setRandomWithFeh
