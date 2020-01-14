# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# ~/.zprofile

# [[ -f ~/.zshrc ]] && . ~/.zshrc
export CHROMIUM_USER_FLAGS="--disk-cache-dir=/tmp --disk-cache-size=50000000"
# export XDG_RUNTIME_DIR="/tmp/XDG_RUNTIME_DIR-$USER"
export XDG_RUNTIME_DIR=/run/user/$(id -u)
export TMP="/tmp"
export TEMP="$TMP"
export TMPDIR="$TMP"
mkdir -m700 -p $XDG_RUNTIME_DIR
