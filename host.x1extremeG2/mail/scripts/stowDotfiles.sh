#!/usr/bin/env bash

set -ex
cd "$(dirname "$0")/config"

add_stow_params=""
# if git diff-index --quiet HEAD --; then
#     echo "git is clean, adopt files ..."
#     add_stow_params="--adopt"
# fi

stow $add_stow_params -t $HOME dotfiles
