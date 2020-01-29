#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"

set -x

nix-build '<nixpkgs/nixos>' \
          $NIX_PATH_ARGS \
          -A system \
          --show-trace --keep-failed \
          --arg configuration "$myconfigDir/default.nix"

times
