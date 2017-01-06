#!/usr/bin/env bash

# nixos-rebuild ###########################################################
if [ -e /etc/nixos/configuration.nix ]; then
    echo "* nixos-rebuild ..."
    [ -x ./nixos/deploy.sh ] \
        && ./nixos/deploy.sh
fi
