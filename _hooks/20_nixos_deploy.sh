#!/usr/bin/env bash

if [ -e /etc/nixos/configuration.nix ]; then
    [ -x ./nixos/deploy.sh ] \
        && ./nixos/deploy.sh
fi
