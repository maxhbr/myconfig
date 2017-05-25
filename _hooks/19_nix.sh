#!/usr/bin/env bash

# nix ######################################################################
if [ -d /etc/nix/ ]; then
    [ -x ./nix/deploy.sh ] \
        && ./nix/deploy.sh
fi
