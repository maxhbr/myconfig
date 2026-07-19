#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# test.sh
#
# Validate a `test-<hostname>` NixOS configuration of this flake without
# modifying the system: run `nix flake check`, then build the system
# toplevel derivation for the host. No result symlink is written.
#
# Usage:
#   ./test.sh [<hostname>]
#
# Arguments:
#   <hostname>  Short host name (without the `test-` prefix) to test against.
#               Defaults to the current machine's hostname.
#
# Examples:
#   # Test the configuration for the current host:
#   ./test.sh
#
#   # Test the configuration for host f13:
#   ./test.sh f13
#
# Notes:
#   * Nothing on the running system is changed (no activation, no profile
#     switch, no out-link). The build artefact is left in the Nix store only.
#   * A log file is written to `../_logs/<date>-myconfig-test-<host>.log`
#     (matching the convention of `switch.sh`) and a symlink
#     `../result.test-<host>.log` points at the most recent run.
#   * Exit status is non-zero if either the flake check or the build fails.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

log_step() {
    {
        echo "$(tput setaf 2)################################################################################$(tput sgr0)"
        echo "$1 (at $(date))"
        echo "$(tput setaf 2)################################################################################$(tput sgr0)"
    } >&2
}
log_info() { echo -e "info: $1" >&2; }
log_warning() { echo -e "$(tput setaf 3)warning: $1$(tput sgr0)" >&2; }
log_error() { echo -e "$(tput setaf 1)error: $1$(tput sgr0)" >&2; }

short_host="${1:-$(hostname 2>/dev/null || cat /proc/sys/kernel/hostname)}"
host_name="test-${short_host}"

if [ "$short_host" = "jail" ]; then
    echo "Refusing to test for host 'jail' (jailed environment); pass an explicit <hostname>." >&2
    exit 1
fi

# Verify the target test host actually exists in the flake.
if ! nix eval --raw ".#nixosConfigurations.\"${host_name}\".config.system.name" >/dev/null 2>&1; then
    log_error "no configuration '${host_name}' found in this flake"
    log_info "available test hosts:"
    nix eval --raw --expr '
      let
        flake = builtins.getFlake ("git+file://" + toString '"${SCRIPT_DIR}"');
      in
        builtins.concatStringsSep "\n"
          (builtins.filter (n: builtins.substring 0 5 n == "test-")
            (builtins.attrNames flake.nixosConfigurations))
    ' --impure 2>/dev/null | sed 's/^/    /' >&2
    exit 1
fi

# Setup logging (mirrors switch.sh convention).
logsDir="../_logs"
mkdir -p "$logsDir"
logfile="$logsDir/$(date +%Y-%m-%d)-myconfig-${host_name}.log"
echo -e "\n\n\n\n\n\n\n" >>"$logfile"
exec > >(tee -a "$logfile") 2>&1

latest_logfile="../result.${host_name}.log"
ln -sf "$(realpath -m --relative-to="$(dirname "$latest_logfile")" "$logfile")" "$latest_logfile"

log_info "starting test of ${host_name}"
log_info "hostname (short): ${short_host}"
log_info "logfile: ${logfile}"
log_info "this script does not modify the system and does not write a result link"

# Optional: authenticate to GitHub to avoid rate limits (best effort).
if command -v pass >/dev/null 2>&1; then
    token="$(pass github-bot-token2 -p 2>/dev/null || true)"
    if [ -n "$token" ]; then
        log_info "setting github token"
        export NIX_CONFIG="access-tokens = github.com=$token"
    else
        log_warning "no github token"
    fi
else
    log_warning "pass not found, skipping github token setup"
fi

status=0

# 1. Run flake checks (evaluates all flake outputs, including tests).
log_step "running 'nix flake check'"
if nix flake check 2>&1; then
    log_info "flake check passed"
else
    rc=$?
    log_error "flake check failed (exit $rc)"
    status=$rc
fi

# 2. Build the system toplevel derivation for the test host (no out-link).
log_step "building toplevel for ${host_name}"
if nix build \
    --no-link \
    --print-out-paths \
    -L \
    --fallback \
    --log-format bar-with-logs \
    ".#nixosConfigurations.\"${host_name}\".config.system.build.toplevel"; then
    log_info "build of ${host_name} toplevel succeeded"
else
    rc=$?
    log_error "build of ${host_name} toplevel failed (exit $rc)"
    status=$rc
fi

log_step "done (exit status ${status})"
exit "$status"
