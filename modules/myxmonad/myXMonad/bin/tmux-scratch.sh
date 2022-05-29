#!/bin/sh
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# ~/.xmonad/tmux-scratch.sh
NAME="tmux-scratch"

tmux has-session -t $NAME 2>/dev/null
[[ "$?" -eq 1 ]] && tmux new-session -d -s $NAME
tmux attach-session -t $NAME
