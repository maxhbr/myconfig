#!/usr/bin/env bash
set -e
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

# rsync file to target folder #############################################
echo "* rsync ..."
sudo rsync --perms -r \
     "$SRC/nixpkgs-config.nix" "$SRC/envs.nix" "$SRC/pkgs" /etc/nix/

# setup channels ##########################################################
echo "* setup channels ..."
nix-channel --add https://nixos.org/channels/nixos-unstable unstable
nix-channel --add https://nixos.org/channels/nixos-17.03 nixos
sudo nix-channel --add https://nixos.org/channels/nixos-unstable unstable
sudo nix-channel --add https://nixos.org/channels/nixos-17.03 nixos
