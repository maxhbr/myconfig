#!/usr/bin/env bash
set -e

# nixos-rebuild ###########################################################
echo "* $(tput bold)nixos-rebuild$(tput sgr0) ..."
exec sudo \
     NIX_CURL_FLAGS='--retry=1000' \
     NIX_PATH='nixpkgs=http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz:unstable=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz:nixpkgs-overlays=/etc/nix/overlays:nixos-config=/etc/nixos/configuration.nix' \
     nixos-rebuild --show-trace \
     -I nixpkgs=http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz \
     -I unstable=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz \
     -I nixpkgs-overlays=/etc/nix/overlays \
     -I nixos-config=/etc/nixos/configuration.nix \
     --upgrade \
     --keep-failed \
     --fallback ${1:-switch}
