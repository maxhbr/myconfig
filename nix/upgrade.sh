#!/usr/bin/env bash
set -e
echo "* $(tput bold)nix-env --upgrade$(tput sgr0) ..."
NIX_PATH=
NIX_CURL_FLAGS='--retry=1000' \
  nix-env -I nixpkgs=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz \
          -I nixpkgs-overlays=/etc/nix/overlays \
          --show-trace \
          --upgrade
