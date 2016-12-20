#!/usr/bin/env bash

image=penrose_4k_color.png

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

rm "$HOME/.desktop-backgronud.png" || continue
ln -s "$DIR/$image" "$HOME/.background-image"

# TODO: --bg-scale or --bg-center depending on dimensions
feh --bg-scale "$HOME/.background-image"
