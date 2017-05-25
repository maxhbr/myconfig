#!/usr/bin/env bash

# nixos-rebuild ###########################################################
if [ -e /etc/nixos/configuration.nix ]; then
    [ -x ./nixos/deploy.sh ] \
        && ./nixos/deploy.sh
fi
