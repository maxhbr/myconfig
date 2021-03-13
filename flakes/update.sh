#!/usr/bin/env bash

set -euo pipefail

find "$(cd "$(dirname "$0")" && pwd)" -maxdepth 1 -mindepth 1 -type d -print0 | 
    while IFS= read -r -d '' flake; do 
        echo "$flake:"
        ( cd "$flake"
          nix flake update)
        ( cd "$flake/../.."
          nix flake update --update-input "$(basename "$flake")"
        )
    done
