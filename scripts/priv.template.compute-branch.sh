#!/usr/bin/env bash
set -euo pipefail

# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Maintain the "priv.template" branch: an orphan-history rebuild of
# ./scripts/priv.template/ produced with `git subtree split`, exactly
# like scripts/priv.template.setup.sh does on the fly. CI runs this
# after every push so that priv.template.setup.sh can import the
# published branch instead of splitting locally.
#
# The split is deterministic, so template commits normally only append
# and the branch fast-forwards. Only a rewrite of myconfig's history
# changes existing template SHAs, hence the anchored --force-with-lease
# instead of a blind force push. Nothing is pushed when the tip is
# unchanged.

myconfig="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$myconfig"

branch="priv.template"
branch_ref="refs/heads/$branch"
prefix="scripts/priv.template"
remote="${1:-origin}"
split_branch="_priv.template-split"

if ! git rev-parse --git-dir >/dev/null 2>&1; then
    echo "Error: Not in a git repository" >&2
    exit 1
fi

if [[ -n "$(git status --porcelain -- "$prefix")" ]]; then
    echo "Error: '$prefix' has uncommitted changes, commit them first" >&2
    git status --short -- "$prefix" >&2
    exit 1
fi

echo "Building template history from $prefix/ ..."
git branch -D "$split_branch" >/dev/null 2>&1 || true
git subtree split --prefix "$prefix" -b "$split_branch" >/dev/null
trap 'git branch -D "$split_branch" >/dev/null 2>&1 || true' EXIT
new_sha="$(git rev-parse "$split_branch")"

old_sha="$(git ls-remote "$remote" "$branch_ref" | cut -f1)"

if [[ $old_sha == "$new_sha" ]]; then
    echo "No template changes ($branch already at $new_sha)"
    exit 0
fi

echo "Updating $branch on $remote: ${old_sha:-<new>} -> $new_sha"
if [[ -z $old_sha ]]; then
    git push "$remote" "$split_branch:$branch_ref"
else
    git push --force-with-lease="$branch_ref:$old_sha" "$remote" "$split_branch:$branch_ref"
fi

# Mirror the pushed state into a local branch; priv.template.setup.sh
# uses it only when it matches the local template state.
git branch -f "$branch" "$new_sha"
