#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

if [ -f ~/.Xresources ] && [ ! -z ${DISPLAY+x} ]; then
    xrdb -merge ~/.Xresources
fi
