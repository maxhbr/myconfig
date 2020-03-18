#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget tmux glibcLocales
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"
set -e

age=${1:-30d}

echo "* nix-env --delete-generations $age ..."
nix-env $NIX_PATH_ARGS \
        --delete-generations $age
echo "* sudo nix-env --delete-generations $age ..."
sudo nix-env $NIX_PATH_ARGS \
             --delete-generations $age
echo "* sudo nix-collect-garbage --delete-older-than $age ..."
sudo nix-collect-garbage \
     --delete-older-than $age
