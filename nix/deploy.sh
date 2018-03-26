#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

# rsync file to target folder #############################################
echo "* $(tput bold)rsync$(tput sgr0) ..."
sudo rsync --perms -r \
     "$SRC/pkgs" "$SRC/overlays" \
     "$SRC/nixpkgs-config.nix" \
     /etc/nix/
# mkdir -p ~/.config/nixpkgs/
# [[ -e ~/.config/nixpkgs/config.nix ]] || \
#     ln -s /etc/nix/nixpkgs-config.nix ~/.config/nixpkgs/config.nix
