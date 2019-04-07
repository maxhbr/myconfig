#!/usr/bin/env nix-shell
#! nix-shell -i bash -p imagemagick

set -e

size=2000

scale() {
    image=$1
    convert "$image"'['"$size"'x]' "${size}_${image}.jpg"
}

for file in "$@"; do
    scale "$file"
done
