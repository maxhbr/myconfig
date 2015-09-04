#!/bin/sh
# ~/.xmonad/tmux-scratch.sh
NAME="tmux-scratch"

tmux has-session -t $NAME 2>/dev/null
[[ "$?" -eq 1 ]] && tmux new-session -d -s $NAME
tmux attach-session -t $NAME
