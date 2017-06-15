#!/usr/bin/env bash
set -e
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

# rsync file to target folder #############################################
echo "* $(tput bold)rsync$(tput sgr0) ..."
sudo rsync --perms -r \
     "$SRC/nixpkgs-config.nix" "$SRC/envs.nix" "$SRC/pkgs" /etc/nix/
mkdir -p ~/.config/nixpkgs/
[[ -e ~/.config/nixpkgs/config.nix ]] || \
    ln -s /etc/nix/nixpkgs-config.nix ~/.config/nixpkgs/config.nix

# setup channels ##########################################################
echo "* $(tput bold)setup channels$(tput sgr0) ..."
nix-channel --add https://nixos.org/channels/nixos-17.03 nixos
nix-channel --add https://nixos.org/channels/nixos-unstable unstable
# nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstabler
sudo nix-channel --add https://nixos.org/channels/nixos-17.03 nixos
sudo nix-channel --add https://nixos.org/channels/nixos-unstable unstable
# sudo nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstabler
