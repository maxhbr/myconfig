#!/bin/sh
# ~/.xmonad/tmux-scratch.sh
PROJECT_NAME="tmux-scratch"

tmux has-session -t $PROJECT_NAME 2>/dev/null
if [ "$?" -eq 1 ] ; then
    echo "No Session found.  Creating and configuring."
    tmux new-session -d -s $PROJECT_NAME
else
    echo "Session found.  Connecting."
fi
tmux attach-session -t $PROJECT_NAME
