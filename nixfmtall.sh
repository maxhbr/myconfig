#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nixfmt
in="$(dirname "$0")"
time find "${1:-"$in"}" \
        -type f \
        -iname '*.nix' \
        -not -iname 'empty_nixos_config.nix' \
        -not -path '*/nixos-hardware/*' \
        -print \
        -exec nixfmt {} \;
