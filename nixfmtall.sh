#!/usr/bin/env bash
time find "$(dirname "$0")" \
        -type f \
        -iname '*.nix' \
        -not -iname 'empty_nixos_config.nix' \
        -not -path '*/nixos-hardware/*' \
        -exec nixfmt {} \;
