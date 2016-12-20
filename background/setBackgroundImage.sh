#!/usr/bin/env bash

image=penrose_4k_color.png

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
target="$HOME/.background-image"

[[ -e "$target" ]] && rm "$target"
ln -s "$DIR/$image" "$target"

# TODO: --bg-scale or --bg-center depending on dimensions
feh --bg-scale "$target"
