#!/usr/bin/env bash
set -e

# nix-env remove old generations ##########################################
type "nix-env" &> /dev/null && {
    nix-env -I nixpkgs=/etc/nix/nixpkgs --upgrade
}
