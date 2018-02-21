#!/usr/bin/env bash

# set desktop background ##################################################
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../.."
src="${ROOT}/background/background-image"
target="$HOME/.background-image"

[[ -e "$target" ]] && rm "$target"
ln -s "$src" "$target"

# TODO: --bg-scale or --bg-center depending on dimensions
feh --bg-scale "$target"
