#!/usr/bin/env bash
set -euo pipefail

target="${1:-$(hostname)}"

cd "$(dirname "$0")"

# ./flakes/update.sh

nix develop \
    --command nix --experimental-features 'nix-command flakes' flake update --update-input "private"
nix develop \
    --command sudo nixos-rebuild switch -p test --flake '.#'"$target"
