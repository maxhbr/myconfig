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
#   ./build-pkg-for-host.sh <pkg-name> [<hostname>] [-- <args...>]
#
# Arguments:
#   <pkg-name>  Name of the package as it appears in `home.packages`
#               (i.e. the derivation's `name` / `pname`). Examples:
#                 agent-bubblewrap-pi, agent-bubblewrap-pi-tmp,
#                 agent-bubblewrap-pi-worktree, pi-bwrap, ...
#   <hostname>  Short host name (without the `test-` prefix) to evaluate
#               against (defaults to the current machine's hostname). The
#               script builds against the `test-<hostname>` key in
#               `self.nixosConfigurations`.
#   -- <args...>  If a `--` separator followed by arguments is given, the
#               built package's main binary is executed with these
#               arguments instead of only printing the output path. The
#               binary is picked from `<out>/bin/`: the single executable
#               there, or the one matching the package name (with any
#               version suffix stripped).
#
# Examples:
#   # Build agent-bubblewrap-pi as configured for the current host:
#   ./build-pkg-for-host.sh agent-bubblewrap-pi
#
#   # Build the pi-bwrap wrapper as configured for host f13:
#   ./build-pkg-for-host.sh pi-bwrap f13
#
#   # Build mysbx and run its binary with arguments:
#   ./build-pkg-for-host.sh mysbx-0.1.0 -- --backend krun --multiplexer none
#
# Notes:
#   * Hard-coded user is `mhuber` (matches `flake.lib.nix`).
#   * Uses `--impure` because the expression imports the current working tree
#     via `builtins.getFlake` on an absolute path.
#   * Picks the *first* matching entry from `home.packages`. If multiple
#     packages share the same `name`, only one is built (rare in practice).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$#" -lt 1 ]; then
    echo "Usage: $(basename "$0") <pkg-name> [<hostname>] [-- <args...>]" >&2
    exit 1
fi

pkg_name="$1"
shift

short_host=""
if [ "$#" -gt 0 ] && [ "$1" != "--" ]; then
    short_host="$1"
    shift
fi

run_args=()
if [ "$#" -gt 0 ] && [ "$1" = "--" ]; then
    shift
    run_args=("$@")
fi

if [ -z "${short_host}" ]; then
    short_host="$(hostname 2>/dev/null || cat /proc/sys/kernel/hostname)"
fi

if [ "${short_host}" = "jail" ]; then
    echo "Refusing to build for host 'jail' (jailed environment); pass an explicit <hostname>." >&2
    exit 1
fi

host_name="test-${short_host}"

echo "==> Building home-manager package '${pkg_name}' for host '${host_name}'"

nix_build_cmd=(
    nix build
    --no-write-lock-file
    --no-link
    --print-out-paths
    --impure
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
)

if [ "${#run_args[@]}" -eq 0 ]; then
    exec "${nix_build_cmd[@]}"
fi

out_path="$("${nix_build_cmd[@]}")"

# Find the binary to run: either the single executable in bin/, or the one
# matching the package name (with any version suffix stripped).
bin_dir="${out_path}/bin"
if [ ! -d "${bin_dir}" ]; then
    echo "ERROR: package '${pkg_name}' has no bin/ directory (${out_path})" >&2
    exit 1
fi

candidates=()
for f in "${bin_dir}"/*; do
    if [ -f "$f" ] && [ -x "$f" ]; then
        candidates+=("$f")
    fi
done

base_name_no_version="$(echo "${pkg_name}" | sed -E 's/-[0-9].*$//')"

binary=""
if [ "${#candidates[@]}" -eq 1 ]; then
    binary="${candidates[0]}"
else
    for c in "${candidates[@]}"; do
        b="$(basename "$c")"
        if [ "${b}" = "${pkg_name}" ] || [ "${b}" = "${base_name_no_version}" ]; then
            binary="$c"
            break
        fi
    done
fi

if [ -z "${binary}" ]; then
    echo "ERROR: could not identify the main binary of '${pkg_name}'." >&2
    echo "Executables in ${bin_dir}:" >&2
    for c in "${candidates[@]}"; do
        echo "  $(basename "$c")" >&2
    done
    exit 1
fi

echo "==> Running: ${binary} ${run_args[*]}"
exec "${binary}" "${run_args[@]}"
