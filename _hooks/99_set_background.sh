#!/usr/bin/env bash

# set desktop background ##################################################
src="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../background/background-image"
target="$HOME/.background-image"

[[ -e "$target" ]] && rm "$target"
ln -s "$src" "$target"

# TODO: --bg-scale or --bg-center depending on dimensions
feh --bg-scale "$target"
