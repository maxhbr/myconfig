#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
./flakes/update.sh

pwd
nix flake update --commit-lock-file
