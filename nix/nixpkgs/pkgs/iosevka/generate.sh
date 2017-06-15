#!/usr/bin/env nix-shell
#!nix-shell -p nodePackages.node2nix -i bash

if [ ! -e package.json ]; then
    echo "./package.json not found"
    echo "Place https://github.com/be5invis/Iosevka/blob/master/package.json"
    echo "in this directory in the correct version and re-run this script."
    exit 1
fi

node2nix -c /dev/null
