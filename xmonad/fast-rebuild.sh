#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
#  NOTE: this does not yet work, it does not actually replace the window manager after building
#
set -e
DIR="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
set -x

result="$(nix-build '<nixpkgs>' -I nixpkgs=$DIR/../nix/nixpkgs --no-out-link --keep-failed -A myconfig.my-xmonad)"

echo -e "\n\n\nresult is $result\n"

$DIR/bin/run-in-xephyr.sh -b $result/bin/xmonad
