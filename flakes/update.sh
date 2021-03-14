#!/usr/bin/env bash

set -euo pipefail

flakes="$(cd "$(dirname "$0")" && pwd)"

( cd "$flakes/myemacs"; nix flake update )
( cd "$flakes/myfish"; nix flake update )
( cd "$flakes/myxmonad"; nix flake update )
