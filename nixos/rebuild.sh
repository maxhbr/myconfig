#!/usr/bin/env bash

SRC=$(dirname $0)
chmod 755 $SRC
sudo rsync --filter="protect /hardware-configuration.nix" \
           --filter="protect /hostname" \
           --filter="protect /hostid" \
           --filter="exclude,s *.gitignore" \
           --filter="exclude,s *.gitmodules" \
           --filter="exclude,s *.git" \
           --filter="exclude .*.swp" \
           --delete --recursive --perms \
           "$SRC" /etc/nixos/

if [ ! -e "/etc/nixos/dotfiles" ]; then
    sudo ln -s  "${SRC}/../dotfiles" "/etc/nixos/dotfiles"
fi
if [ ! -e "/etc/nixos/backgrounds" ]; then
    sudo ln -s  "${SRC}/../backgrounds" "/etc/nixos/backgrounds"
fi

sudo \
    NIX_CURL_FLAGS='--retry=1000' \
    nixos-rebuild --show-trace \
                  --upgrade \
                  --keep-failed \
                  --fallback ${1:-switch}
