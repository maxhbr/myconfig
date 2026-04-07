#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Fetch a remote's master and create a GitHub PR to merge it into origin/master.
# Optionally excludes flake.lock changes to avoid merge conflicts.
#
# Usage: create-pr-from-remote.sh [REMOTE] [--include-flake-lock]
#   REMOTE               git remote name (default: p14)
#   --include-flake-lock  keep flake.lock changes (default: excluded)

set -euo pipefail
cd "$(dirname "$0")/.."

REMOTE="${1:-p14}"
INCLUDE_FLAKE_LOCK=false

for arg in "$@"; do
    case "$arg" in
        --include-flake-lock)
            INCLUDE_FLAKE_LOCK=true
            ;;
    esac
done

BRANCH="merge/${REMOTE}-master"
TIMESTAMP="$(date +%Y-%m-%d-%H%M)"
BRANCH="${BRANCH}-${TIMESTAMP}"

echo "==> Fetching remote '${REMOTE}'..."
git fetch "${REMOTE}"

REMOTE_REF="${REMOTE}/master"
LOCAL_MASTER="origin/master"

# Check if there are any differences
if git diff --quiet "${LOCAL_MASTER}..${REMOTE_REF}" --; then
    echo "No differences between ${LOCAL_MASTER} and ${REMOTE_REF}. Nothing to do."
    exit 0
fi

ORIGINAL_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
cleanup() {
    echo "==> Switching back to ${ORIGINAL_BRANCH}..."
    git checkout "${ORIGINAL_BRANCH}"
}
trap cleanup EXIT

echo "==> Creating branch '${BRANCH}' from '${REMOTE_REF}'..."
git checkout -b "${BRANCH}" "${REMOTE_REF}"

if [[ ${INCLUDE_FLAKE_LOCK} == "false" ]]; then
    # Reset flake.lock to the state on origin/master to avoid conflicts
    if git diff --quiet "${LOCAL_MASTER}" "${REMOTE_REF}" -- flake.lock; then
        echo "==> flake.lock is identical, nothing to filter."
    else
        echo "==> Resetting flake.lock to origin/master state..."
        git checkout "${LOCAL_MASTER}" -- flake.lock
        git commit -m "reset flake.lock to origin/master state"
    fi
fi

echo "==> Pushing branch '${BRANCH}' to origin..."
git push -u origin "${BRANCH}"

echo "==> Creating pull request..."
gh pr create \
    --base master \
    --head "${BRANCH}" \
    --title "Merge ${REMOTE}/master (${TIMESTAMP})" \
    --body "$(
        cat <<EOF
## Summary

- Merge changes from \`${REMOTE}/master\` into \`master\`
- Source remote: \`${REMOTE}\` (\`$(git remote get-url "${REMOTE}")\`)
$(if [[ ${INCLUDE_FLAKE_LOCK} == "false" ]]; then echo "- **flake.lock changes excluded** to avoid merge conflicts"; fi)
EOF
    )"

echo "==> Done."
