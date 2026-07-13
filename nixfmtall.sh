#!/usr/bin/env bash
set -euo pipefail

# Vendored subtrees are managed via `git subtree` and keep their upstream
# formatting so that `git subtree pull` merges stay clean. Exclude them from
# treefmt so `nix fmt` does not reformat them in place.
export TREEFMT_EXCLUDES="vendor/**"

if [[ ${1:-} == "--check" ]]; then
    # treefmt's --fail-on-change exits non-zero if any file would change.
    nix fmt -- --fail-on-change
else
    exec nix fmt
fi
