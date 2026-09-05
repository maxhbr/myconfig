#!/usr/bin/env bash
set -euo pipefail

# Formatter excludes live in `formatterExcludeDirs` in flake.nix.

if [[ ${1:-} == "--check" ]]; then
    # treefmt's --fail-on-change exits non-zero if any file would change.
    nix fmt -- --fail-on-change
else
    exec nix fmt
fi
