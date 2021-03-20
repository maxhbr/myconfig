#!/usr/bin/env bash

set -euo pipefail

flakes="$(cd "$(dirname "$0")" && pwd)"

( cd "$flakes/myemacs"; nix flake update --commit-lock-file )
( cd "$flakes/myfish"; nix flake update --commit-lock-file )
( cd "$flakes/myxmonad"; nix flake update --commit-lock-file )
( cd "$flakes/zephyrproject"; nix flake update --commit-lock-file )
