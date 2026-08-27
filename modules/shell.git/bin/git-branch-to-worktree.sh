#!/usr/bin/env bash
# Copyright 2024 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# git-branch-to-worktree: move a branch out of the main checkout into a
# workmux-style git worktree (a sibling directory
# `../<repo>__worktrees/<slugified-branch>`).
set -euo pipefail

self="git-branch-to-worktree"

usage() {
    cat <<'EOF'
Usage: git branch-to-worktree [<branch>]

Convert a branch into a workmux-style git worktree, so that work started
directly in the main checkout can be moved into its own worktree.

  <branch>    Branch to move into a worktree. If the branch does not exist
              yet it is created from the CURRENT HEAD (that is usually what
              you want mid-work), not from the default branch.
              If omitted, the currently checked out branch is used; this is
              an error when the current branch is the default branch
              (origin's HEAD, else "main", else "master").

  -h, --help  Show this help.

The worktree is created at

    <parent-of-repo>/<repo-name>__worktrees/<handle>

where <handle> is the branch name slugified the same way workmux does
(lowercased, every run of non-alphanumeric characters replaced by a single
"-", leading/trailing "-" removed), e.g. "feature/foo bar" -> "feature-foo-bar".

If the branch is currently checked out in the main checkout, the main
checkout is switched to the default branch first (git refuses to create a
worktree for an already checked out branch). Uncommitted changes are never
touched: the command refuses to run with a dirty working tree in that case.
EOF
}

die() {
    echo "$self: $*" >&2
    exit 1
}

# Same slugification as workmux's `derive_handle` (slug::slugify) for plain
# ASCII branch names; non-ASCII characters are turned into "-" instead of
# being transliterated.
slugify() {
    local input="${1,,}" out="" ch i
    for ((i = 0; i < ${#input}; i++)); do
        ch="${input:i:1}"
        case "$ch" in
            [a-z0-9]) out+="$ch" ;;
            *) out+="-" ;;
        esac
    done
    while [[ $out == *--* ]]; do
        out="${out//--/-}"
    done
    out="${out#-}"
    out="${out%-}"
    printf '%s\n' "$out"
}

# Path of the MAIN working tree (the first entry of `git worktree list`),
# even when this command is run from within a linked worktree.
main_worktree() {
    git worktree list --porcelain | awk '/^worktree /{print substr($0, 10); exit}'
}

branch_exists() {
    git show-ref --verify --quiet "refs/heads/$1"
}

# Working tree path in which $1 is checked out, empty if nowhere.
worktree_of_branch() {
    git worktree list --porcelain | awk -v ref="refs/heads/$1" '
        /^worktree /{ path = substr($0, 10) }
        $0 == "branch " ref { print path; exit }
    '
}

detect_default_branch() {
    local sym candidate
    sym="$(git symbolic-ref --short --quiet refs/remotes/origin/HEAD 2>/dev/null || true)"
    for candidate in "${sym#origin/}" main master; do
        [ -n "$candidate" ] || continue
        if branch_exists "$candidate" ||
            git show-ref --verify --quiet "refs/remotes/origin/$candidate"; then
            printf '%s\n' "$candidate"
            return 0
        fi
    done
    return 1
}

branch=""
while [ $# -gt 0 ]; do
    case "$1" in
        -h | --help)
            usage
            exit 0
            ;;
        -*)
            echo "$self: unknown option: $1" >&2
            usage >&2
            exit 1
            ;;
        *)
            [ -z "$branch" ] || die "too many arguments: $1"
            branch="$1"
            ;;
    esac
    shift
done

git rev-parse --git-dir >/dev/null 2>&1 || die "not inside a git repository"

repo_root="$(main_worktree)"
[ -n "$repo_root" ] || die "could not determine the main working tree"

default_branch="$(detect_default_branch || true)"
[ -n "$default_branch" ] ||
    die "could not determine the default branch (tried origin's HEAD, main, master)"

current_branch="$(git symbolic-ref --short --quiet HEAD || true)"

if [ -z "$branch" ]; then
    [ -n "$current_branch" ] ||
        die "HEAD is detached and no branch was given; usage: $self <branch>"
    if [ "$current_branch" = "$default_branch" ]; then
        die "refusing to move the default branch ($default_branch) into a worktree; pass a branch name to create one instead"
    fi
    branch="$current_branch"
fi

git check-ref-format --branch "$branch" >/dev/null 2>&1 ||
    die "not a valid branch name: $branch"

handle="$(slugify "$branch")"
[ -n "$handle" ] || die "branch name slugifies to the empty string: $branch"

worktrees_dir="$(dirname "$repo_root")/$(basename "$repo_root")__worktrees"
target="$worktrees_dir/$handle"

[ ! -e "$target" ] || die "target worktree path already exists: $target"

existing_worktree=""
if branch_exists "$branch"; then
    existing_worktree="$(worktree_of_branch "$branch")"
fi

if [ -n "$existing_worktree" ] && [ "$existing_worktree" != "$repo_root" ]; then
    die "branch '$branch' is already checked out in another worktree: $existing_worktree"
fi

mkdir -p "$worktrees_dir"

if [ -n "$existing_worktree" ]; then
    # The branch is checked out in the main checkout: git refuses to create a
    # worktree for it, so free it up by switching to the default branch first.
    if [ -n "$(git -C "$repo_root" status --porcelain)" ]; then
        die "the main checkout ($repo_root) has uncommitted changes; commit or stash them first (nothing was changed)"
    fi
    git -C "$repo_root" switch "$default_branch"
    if ! git -C "$repo_root" worktree add "$target" "$branch"; then
        git -C "$repo_root" switch "$branch" || true
        die "failed to create worktree at $target (main checkout restored to '$branch')"
    fi
elif branch_exists "$branch"; then
    git -C "$repo_root" worktree add "$target" "$branch"
else
    # New branch: fork it off the current HEAD, which is what one wants when
    # moving mid-work changes/commits out of the main checkout.
    git worktree add -b "$branch" "$target" HEAD
fi

printf '%s\n' "$target"
