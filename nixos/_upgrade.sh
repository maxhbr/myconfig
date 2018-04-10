#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

# nixos-rebuild ###########################################################
echo "* $(tput bold)nixos-rebuild$(tput sgr0) ..."
NIX_PATH=
exec sudo \
     NIX_CURL_FLAGS='--retry=1000' \
     nixos-rebuild --show-trace --keep-failed \
     -I nixpkgs=http://nixos.org/channels/nixos-18.03/nixexprs.tar.xz \
     -I nixpkgs-overlays=/etc/nix/overlays \
     -I nixos-config=/etc/nixos/configuration.nix \
     --upgrade \
     --fallback ${1:-switch}
