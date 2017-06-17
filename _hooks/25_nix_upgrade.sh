#!/usr/bin/env bash

# nix ######################################################################
if [ -d /etc/nix/ ]; then
    type "nix-env" &> /dev/null && {
        [ -x ./nix/upgrade.sh ] \
            && ./nix/upgrade.sh
    }
fi
