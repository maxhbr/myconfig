#!/usr/bin/env bash
# Copyright 2017-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# set desktop background ##################################################
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../.."
$ROOT/background/randomizeBackground.sh
src="${ROOT}/background/background-image"
target="$HOME/.background-image"

[[ -e "$target" ]] && rm "$target"
ln -s "$src" "$target"

# TODO: --bg-scale or --bg-center depending on dimensions
feh --bg-scale "$target"
