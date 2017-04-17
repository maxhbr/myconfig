# ~/.zprofile
#
# written by maximilian-huber.de
#
# Last modified: Thu Apr 09, 2015  01:25

# [[ -f ~/.zshrc ]] && . ~/.zshrc
export CHROMIUM_USER_FLAGS="--disk-cache-dir=/tmp --disk-cache-size=50000000"
export XDG_RUNTIME_DIR="/tmp/XDG_RUNTIME_DIR-$USER"
mkdir -m700 -p $XDG_RUNTIME_DIR
