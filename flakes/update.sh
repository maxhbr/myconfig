#!/usr/bin/env bash

set -euo pipefail

flakes="$(cd "$(dirname "$0")" && pwd)"

# ( cd "$flakes/myemacs"; pwd; nix flake update --commit-lock-file )
( cd "$flakes/myfish"; pwd; nix flake update --commit-lock-file )
( cd "$flakes/myxmonad"; pwd; nix flake update --commit-lock-file )
( cd "$flakes/zephyrproject"; pwd; nix flake update --commit-lock-file )
( cd "$flakes/mykeylight"; pwd; nix flake update --commit-lock-file )
