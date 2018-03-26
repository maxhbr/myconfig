# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# ~/.zprofile

# [[ -f ~/.zshrc ]] && . ~/.zshrc
export CHROMIUM_USER_FLAGS="--disk-cache-dir=/tmp --disk-cache-size=50000000"
export XDG_RUNTIME_DIR="/tmp/XDG_RUNTIME_DIR-$USER"
mkdir -m700 -p $XDG_RUNTIME_DIR
