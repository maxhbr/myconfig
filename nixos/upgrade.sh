#!/usr/bin/env bash
set -e


# nixos-rebuild ###########################################################
echo "* $(tput bold)nixos-rebuild$(tput sgr0) ..."
NIX_PATH=
exec sudo \
     NIX_CURL_FLAGS='--retry=1000' \
     nixos-rebuild --show-trace \
     -I nixpkgs=http://nixos.org/channels/nixos-17.09/nixexprs.tar.xz \
     -I nixpkgs-overlays=/etc/nix/overlays \
     -I nixos-config=/etc/nixos/configuration.nix \
     --upgrade \
     --keep-failed \
     --fallback ${1:-switch}
