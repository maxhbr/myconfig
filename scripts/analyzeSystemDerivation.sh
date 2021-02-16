#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix-du graphviz
# Copyright 2021 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e
set -o pipefail
shopt -s lastpipe

out="/tmp/nix-du"
mkdir -p "$out"

nix-du -r "${1:-/run/current-system/}" | dot -Tsvg -o "$out/nix-du.svg"
echo "see $out/nix-du.svg"
