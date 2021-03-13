#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

if [[ -f ../switch.sh ]]; then
  exit 1
fi

target="${1:-$(hostname)}"

if [ $# -gt 0 ]; then
    targetIP="$(cat "./secrets/$target/ip")"
    echo "No arguments supplied"
else
    targetIP="localhost"
fi

./update.sh

set -x
nix develop \
    --command sudo nixos-rebuild \
    --target-host "$targetIP" \
    switch `#-p test` \
    --flake '.#'"$target"

set +x

# ./scripts/gc.sh
