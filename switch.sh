#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

target="${1:-$(hostname)}"

if [ $# -gt 0 ]; then
    targetIP="$(cat "./secrets/$target/ip")"
    echo "No arguments supplied"
else
    targetIP="localhost"
fi

./flakes/update.sh

set -x

nix develop \
    --command nix flake update --update-input "private"
nix develop \
    --command sudo nixos-rebuild \
    --target-host "$targetIP" \
    switch -p test \
    --flake '.#'"$target"

set +x

# ./scripts/gc.sh
