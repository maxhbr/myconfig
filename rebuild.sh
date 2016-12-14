#!/usr/bin/env bash

SRC=$(dirname $0)
chmod 755 $SRC
sudo rsync --filter="protect /hardware-configuration.nix" \
           --filter="protect /hostname" \
           --filter="protect /hostid" \
           --filter="protect /dotfiles" \
           --filter="exclude,s *.gitignore" \
           --filter="exclude,s *.gitmodules" \
           --filter="exclude,s *.git" \
           --filter="exclude .*.swp" \
           --filter="exclude Session.vim" \
           --exclude 'dotfiles' \
           --delete --recursive --perms \
           "$SRC" /etc/nixos/

sudo ln -s  dotfiles /etc/nixos/dotfiles

sudo \
    NIX_CURL_FLAGS='--retry=1000' \
    nixos-rebuild --show-trace \
                  --upgrade \
                  --keep-failed \
                  --fallback ${1:-switch}
