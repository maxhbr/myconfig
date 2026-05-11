#!/usr/bin/env bash
# build-pkg-for-host.sh
#
# Build a single home-manager package as it would be installed for a given
# host. This is the user-level analogue of
#   nix build .#nixosConfigurations.<host>.config.system.build.toplevel
# but for one specific entry of `home.packages` (the user `mhuber`).
#
# It is useful while developing a module that wires up new packages (e.g. a
# bubblewrap/jail wrapper) without having to rebuild the entire system or
# home-manager closure.
#
# Usage:
#   ./build-pkg-for-host.sh <pkg-name> [<hostname>]
#
# Arguments:
#   <pkg-name>  Name of the package as it appears in `home.packages`
#               (i.e. the derivation's `name` / `pname`). Examples:
#                 jailed-pi, jailed-pi-tmp, jailed-pi-worktree, pi-bwrap, ...
#   <hostname>  NixOS host name to evaluate against (defaults to the current
#               machine's hostname, see `hostname`). Must match a key in
#               `self.nixosConfigurations`.
#
# Examples:
#   # Build jailed-pi as configured for the current host:
#   ./build-pkg-for-host.sh jailed-pi
#
#   # Build the pi-bwrap wrapper as configured for host f13:
#   ./build-pkg-for-host.sh pi-bwrap f13
#
# Notes:
#   * Hard-coded user is `mhuber` (matches `flake.lib.nix`).
#   * Uses `--impure` because the expression imports the current working tree
#     via `builtins.getFlake` on an absolute path.
#   * Picks the *first* matching entry from `home.packages`. If multiple
#     packages share the same `name`, only one is built (rare in practice).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
    echo "Usage: $(basename "$0") <pkg-name> [<hostname>]" >&2
    exit 1
fi

pkg_name="$1"
host_name="${2:-$(hostname)}"

echo "==> Building home-manager package '${pkg_name}' for host '${host_name}'"

exec nix build \
    --no-write-lock-file \
    --no-link \
    --print-out-paths \
    --impure \
    --expr "
let
  flake = builtins.getFlake (\"git+file://\" + toString ${SCRIPT_DIR});
  cfg = flake.nixosConfigurations.\"${host_name}\";
  ps = cfg.config.home-manager.users.mhuber.home.packages;
  matches = builtins.filter (p: (p.name or p.pname or \"\") == \"${pkg_name}\") ps;
in
  if matches == [] then
    throw \"No package named '${pkg_name}' in home.packages of host '${host_name}'\"
  else
    builtins.head matches
"
