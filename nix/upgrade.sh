#!/usr/bin/env bash
set -e

echo "* $(tput bold)nix-env --upgrade$(tput sgr0) ..."
env \
    NIX_CURL_FLAGS='--retry=1000' \
    NIX_PATH='nixpkgs=http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz:unstable=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz:nixpkgs-overlays=/etc/nix/overlays:nixos-config=/etc/nixos/configuration.nix' \
            nix-env \
            -I nixpkgs=http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz \
            -I unstable=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz \
            -I nixpkgs-overlays=/etc/nix/overlays \
            -I nixos-config=/etc/nixos/configuration.nix \
            --upgrade
