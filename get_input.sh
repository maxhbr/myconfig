#!/usr/bin/env bash
# get_input.sh
#
# Print the nix store path of the pinned (flake.lock) version of a flake input.
#
# Usage:
#   ./get_input.sh <input-name>
#
# Arguments:
#   <input-name>  Name of a top-level flake input as declared in `flake.nix`
#                 and pinned in `flake.lock`. Examples:
#                   nixpkgs, home, agenix, nur, nixos-hardware, ...
#
# Example:
#   $ ./get_input.sh nixpkgs
#   /nix/store/d1qrs4cfnhmkhkripp32pyn64c8ldjzq-source
#
# The returned path is the materialised source tree of the locked revision,
# the same store path that `nix build` / `nix develop` would consume. Inputs
# that `follows` another (e.g. `home.inputs.nixpkgs.follows = "nixpkgs"`) and
# non-flake inputs (`flake = false`) are supported.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$#" -ne 1 ]; then
    echo "Usage: $(basename "$0") <input-name>" >&2
    exit 1
fi

input_name="$1"

nix eval \
    --raw \
    --impure \
    --expr "
let
  flake = builtins.getFlake (\"git+file://\" + toString ${SCRIPT_DIR});
  input = flake.inputs.\"${input_name}\" or
    (throw \"flake input '${input_name}' does not exist\");
in
  input.outPath
"
