#!/usr/bin/env bash

# nix ######################################################################
if [ -d /etc/nix/ ]; then
    echo "* nix ..."
    [ -x ./nix/deploy.sh ] \
        && ./nix/deploy.sh
fi
