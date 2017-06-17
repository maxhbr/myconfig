#!/usr/bin/env bash

if [ -e /etc/nixos/configuration.nix ]; then
    [ -x ./nixos/upgrade.sh ] \
        && ./nixos/upgrade.sh
fi
