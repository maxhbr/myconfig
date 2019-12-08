#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"
if have xrdb; then
    if [ -f ~/.Xresources ] && [ ! -z ${DISPLAY+x} ]; then
        logH3 "run xrdb -merge ~/.Xresources"
        xrdb -merge ~/.Xresources
    fi
fi

