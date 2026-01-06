#!/usr/bin/env bash
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

# build() {
#     local hostname="$1"
#     local out=../sd-image."${hostname}"
#     local oldOut=""
#     if [[ -d "$oldOut" ]]; then
#         oldOut="$(readlink -f "$out")"
#     fi
#     (set -x; time nix build --out-link "$out" --show-trace .#"${hostname}"-sd-image)
#     local newOut="$(readlink -f "$out")"
#     if [[ -n $oldOut && -d "$oldOut" && "$oldOut" != "$newOut" ]]; then
#         (set -x; nix store delete "$oldOut")
#     fi
# }

build() {
    local hostname="$1"
    local out=../sd-image."${hostname}"
    nix run nixpkgs#nixos-generators -- -f sd-aarch64 --flake .#"${hostname}" --system aarch64-linux -o "$out"
}

# nix flake update
# [[ -d "~/myconfig/priv" ]] && {
#   cd "~/myconfig/priv"
#   nix flake update
# }

build "${1:-pi4}"
