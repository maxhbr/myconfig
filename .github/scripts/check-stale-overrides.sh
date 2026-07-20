#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# check-stale-overrides.sh
#
# Detect nixpkgs-override workarounds in flake.pkgs_overrides.nix that have
# become redundant: the upstream nixpkgs commit that fixes the underlying bug
# is now reachable from the flake's pinned nixpkgs-unstable input.
#
# Each override in flake.pkgs_overrides.nix carries a "TODO: remove once the
# nixpkgs input includes commit <SHA>" note. The SHAs listed in
# TRACKED_COMMITS below are those notes made machine-checkable: this script
# asks GitHub whether <SHA> is an ancestor of the pinned nixpkgs revision
# (read from flake.lock). If it is, the override is stale and the script
# exits non-zero so CI fails until the override (and its entry here) is
# removed.
#
# Why check the *pinned* revision and not the nixpkgs-unstable branch tip:
# the override only becomes *safe to remove* once the flake's own nixpkgs
# input includes the fix. Failing on the branch tip instead would nag to
# remove an override that the (still-behind) pin still needs, breaking the
# build. Checking the pin means CI fails exactly when removal is safe —
# right after a `nix flake update` that pulled in the fix.
#
# Usage:
#   ./check-stale-overrides.sh
#
# Set GITHUB_TOKEN (or GH_TOKEN) to raise the API rate limit; without a token
# the unauthenticated limit (60 req/h) is plenty for a handful of entries.
set -euo pipefail

# Repo root is two levels up from .github/scripts/.
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FLAKE_LOCK="$REPO_ROOT/flake.lock"

if ! command -v jq >/dev/null 2>&1; then
    echo "ERROR: jq is required but not found on PATH." >&2
    exit 1
fi
if ! command -v curl >/dev/null 2>&1; then
    echo "ERROR: curl is required but not found on PATH." >&2
    exit 1
fi

# Pinned revision of the flake's nixpkgs input (tracks nixpkgs-unstable).
PINNED_REV="$(jq -r '.nodes.nixpkgs.locked.rev' "$FLAKE_LOCK")"
if [ -z "$PINNED_REV" ] || [ "$PINNED_REV" = "null" ]; then
    echo "ERROR: could not read .nodes.nixpkgs.locked.rev from $FLAKE_LOCK" >&2
    exit 1
fi

# Nixpkgs commits that make a local override redundant once they are reachable
# from the pinned nixpkgs input. Format: "FIX_SHA|description".
#
# Add an entry here in lockstep with every "TODO: remove once the nixpkgs
# input includes commit <SHA>" note added to flake.pkgs_overrides.nix, and
# remove the entry together with its override.
TRACKED_COMMITS=(
    "89d71ccd4648d644a6d093cc6af61a85a113ecf6|pass-secret-service 0-unstable-2026-07-15 (override in flake.pkgs_overrides.nix)"
)

# Query the GitHub compare API. `compare/{base}...{head}` returns, for head
# relative to base:
#   behind_by == 0  ->  base is an ancestor of head (or equal)
#   behind_by  > 0  ->  base is NOT an ancestor of head
# We want to know whether FIX_SHA is an ancestor of PINNED_REV, so base is
# the fix commit and head is the pinned revision.
github_compare() {
    local base="$1" head="$2"
    local auth=()
    local token="${GITHUB_TOKEN:-${GH_TOKEN:-}}"
    if [ -n "$token" ]; then
        auth=(-H "Authorization: Bearer $token")
    fi
    curl --retry 3 --retry-delay 2 --retry-connrefused -fsSL \
        "${auth[@]}" -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" \
        "https://api.github.com/repos/NixOS/nixpkgs/compare/${base}...${head}"
}

stale=0
for entry in "${TRACKED_COMMITS[@]}"; do
    fix_sha="${entry%%|*}"
    desc="${entry#*|}"

    resp="$(github_compare "$fix_sha" "$PINNED_REV")" || {
        echo "ERROR: GitHub compare API call failed for ${fix_sha}...${PINNED_REV}." >&2
        echo "       Check network connectivity and API rate limits." >&2
        exit 1
    }

    behind="$(printf '%s' "$resp" | jq -r '.behind_by // empty')"
    if [ -z "$behind" ]; then
        echo "ERROR: could not parse '.behind_by' from GitHub compare response:" >&2
        printf '%s\n' "$resp" >&2
        exit 1
    fi

    if [ "$behind" -eq 0 ]; then
        echo "STALE: nixpkgs commit ${fix_sha} (${desc}) is reachable from the" >&2
        echo "       pinned nixpkgs-unstable input (${PINNED_REV})." >&2
        echo "       The override is now redundant — remove it and this entry." >&2
        stale=1
    else
        echo "ok: ${desc}: not yet in pinned nixpkgs (pin is ${behind} commits behind the fix)"
    fi
done

exit "$stale"
