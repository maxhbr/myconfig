#!/usr/bin/env bash

# set desktop background ##################################################
image=penrose_4k_color.png
image=romben.png

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../background"
target="$HOME/.background-image"

[[ -e "$target" ]] && rm "$target"
ln -s "$DIR/$image" "$target"

# TODO: --bg-scale or --bg-center depending on dimensions
feh --bg-scale "$target"
