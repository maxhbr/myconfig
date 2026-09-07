#!/usr/bin/env bash
set -euo pipefail

# `nix fmt` is treefmt: nixfmt for *.nix, rustfmt for *.rs (the Rust crates
# under modules/). Both the formatter set and the excludes
# (`formatterExcludeDirs`) live in the `formatter` attribute of flake.nix.

if [[ ${1:-} == "--check" ]]; then
    # treefmt's --fail-on-change exits non-zero if any file would change.
    nix fmt -- --fail-on-change
else
    exec nix fmt
fi
