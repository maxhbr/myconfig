#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# master-to-main.sh — catch up a local checkout to the master→main branch
# migration.
#
# This script is meant to be distributed to all machines *before* the
# upstream default-branch migration (master→main) and run once *after* it,
# in every local checkout. It is purely local: it never pushes anything.
#
# What it does (when run on branch `master`):
#   1. Verifies the worktree is clean (staged and unstaged changes are
#      refused — commit or stash them first).
#   2. Fetches origin (with --prune) and verifies that the remote has
#      actually migrated (no 'origin/master' anymore). Verifies that local
#      'master' is not behind 'origin/main'. If it is behind, the script
#      aborts with instructions instead of pulling — a pull can fail or
#      create conflicts, and a catch-up script must not leave the machine
#      in a half-updated state.
#   3. Checks a pre-existing local 'main':
#        - a 'main' whose commits are all already contained in local
#          'master' (behind, equal, or fully merged) is deleted with
#          'git branch -D' — after printing an explicit warning;
#        - a 'main' with commits that are not in local 'master' (diverged
#          or ahead) is kept and the script aborts, so nothing on it can
#          be lost;
#        - a 'main' that is currently checked out cannot happen here (we
#          are on 'master'), but if the user runs the script from a
#          checkout on a diverged 'main', that is reported as an error
#          they must resolve.
#   4. Renames the branch: 'git branch -m master main' (the checkout ends
#      up on 'main') and points its upstream at 'origin/main'.
#   5. Removes a leftover local 'master' ref, if any (e.g. from an
#      aborted earlier run).
#
# Running it again after a successful migration is an idempotent no-op.
#
# Usage: master-to-main.sh [--help]

set -euo pipefail

SCRIPT_NAME="$(basename "$0")"

log() {
    echo "==> $*"
}

warn() {
    echo "WARNING: $*" >&2
}

die() {
    echo "ERROR: $*" >&2
    echo "Aborting. No changes have been made." >&2
    exit 1
}

usage() {
    cat <<EOF
${SCRIPT_NAME} — catch up this checkout to the master→main branch migration.

Runs entirely locally (no pushing). Safe to re-run on an already migrated
checkout (idempotent no-op). Refuses to touch anything if the worktree is
dirty or if deleting a local 'main' branch could lose commits.

Usage:
  ${SCRIPT_NAME} [--help]

Options:
  --help    print this help and exit

Exit codes:
  0  success (migrated, or already migrated)
  1  aborted (dirty worktree, local 'master' behind, diverged local 'main', ...)
EOF
}

for arg in "$@"; do
    case "$arg" in
        --help | -h)
            usage
            exit 0
            ;;
        *)
            usage >&2
            exit 1
            ;;
    esac
done

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Resolve the top-level directory of the git checkout and work from there.
repo_toplevel="$(git rev-parse --show-toplevel)" ||
    die "not a git repository (or any of the parent directories)"
cd "${repo_toplevel}"

# True if the local branch <name> exists.
has_local_branch() {
    git show-ref --verify --quiet "refs/heads/$1"
}

# True if the remote-tracking ref origin/<name> exists.
has_remote_branch() {
    git show-ref --verify --quiet "refs/remotes/origin/$1"
}

# True if commit <ancestor> is contained in (is an ancestor of, or equal
# to) commit <descendant>.
is_ancestor() {
    # shellcheck disable=SC2312 # invoked only via if, exit code not masked
    git merge-base --is-ancestor "$1" "$2"
}

# Abort if the worktree has staged or unstaged changes to tracked files.
# Untracked files are ignored (e.g. nix 'result' symlinks): a branch
# rename never touches the worktree, so they are harmless here.
require_clean_worktree() {
    if ! git rev-parse --verify --quiet HEAD >/dev/null; then
        die "this repository has no commits yet — nothing to migrate"
    fi
    if ! git diff-index --quiet --ignore-submodules HEAD --; then
        git status --short >&2
        die "the working tree has uncommitted changes to tracked files
(see above) — commit or stash them first"
    fi
}

# Print the short name of the branch currently checked out, or nothing if
# HEAD is detached.
current_branch() {
    # shellcheck disable=SC2312 # invoked only via $(), failure handled by caller
    git symbolic-ref --quiet --short HEAD
}

# Fetch origin with --prune, warning (not aborting) if the remote is not
# reachable: the migration can still proceed if the local refs already
# reflect the migrated state.
fetch_origin() {
    if ! git fetch --prune origin; then
        warn "could not fetch 'origin' (offline?) — continuing with the refs
that are present locally"
    fi
}

# ---------------------------------------------------------------------------
# 1. Which branch are we on?
# ---------------------------------------------------------------------------

CUR="$(current_branch)" || CUR=""
if [ -z "${CUR}" ]; then
    die "HEAD is detached (not on any branch) — check out 'master' (or 'main' if already migrated) first"
fi
log "On branch '${CUR}'"

if [ "${CUR}" = "main" ]; then
    # ---------------------------------------------------------------
    # Already on 'main': idempotent success if the migration is done or
    # the branch is not behind the remote, otherwise an error the user
    # must resolve.
    # ---------------------------------------------------------------
    # All checks below are read-only (fetch + merge-base), so a dirty
    # worktree is fine here: unlike the migration path, nothing is
    # renamed or deleted in this mode.
    log "Already on 'main' — checking whether this checkout is up to date."
    fetch_origin

    if has_remote_branch "master" && has_remote_branch "main"; then
        die "'origin' has both 'master' and 'main' — the migration on the
remote seems to be in progress or incomplete. Wait until 'origin/master' is
gone and re-run this script."
    fi
    if has_remote_branch "master"; then
        die "the remote still uses 'master', but this checkout is on a local
'main'. That 'main' is probably unrelated to the migration. Switch to
'master' (git checkout master) and re-run this script."
    fi
    if ! has_remote_branch "main"; then
        log "Remote branch 'origin/main' not found (yet). Nothing to do."
        log "This checkout is already on 'main'. Keeping it as is."
        log "Note: if the remote master→main migration has not happened yet,
re-run this script after it has."
        exit 0
    fi
    if is_ancestor "origin/main" "main"; then
        log "'main' is up to date with 'origin/main'. Nothing to do."
        exit 0
    fi
    die "'main' is behind 'origin/main'.
Resolve this manually, e.g. in this checkout run:
    git pull --ff-only
and then re-run ${SCRIPT_NAME}."
fi

if [ "${CUR}" != "master" ]; then
    die "this checkout is on branch '${CUR}', not 'master' or 'main'.
Check out 'master' (git checkout master) and re-run this script,
or 'main' if it is already migrated."
fi

# ---------------------------------------------------------------------------
# 2. On 'master': make sure the remote has migrated and 'master' is not
#    behind the new 'main'
# ---------------------------------------------------------------------------

require_clean_worktree

log "Fetching 'origin' (with --prune)..."
fetch_origin

if has_remote_branch "master" && has_remote_branch "main"; then
    die "'origin' has both 'master' and 'main' — the migration on the remote
seems to be in progress or incomplete. Wait until 'origin/master' is gone
and re-run this script."
fi
if has_remote_branch "master"; then
    die "the remote 'origin' still has 'master' — the upstream master→main
migration has not happened yet (or this machine cannot fetch and its
remote-tracking refs are stale). Renaming now would disconnect this
checkout from the actual remote branch, so nothing was changed.
Re-run this script once the upstream migration is done."
fi

# The remote master is gone: the new reference point is 'origin/main'.
BASE_REF="refs/remotes/origin/main"
if has_remote_branch "main"; then
    if ! is_ancestor "${BASE_REF}" "master"; then
        die "local 'master' is behind 'origin/main' (the remote has commits
that local 'master' does not). Update it first, then re-run this script:
    git pull --ff-only origin main
(The script does not pull on its own, so it cannot leave the machine in a
half-updated state.)"
    fi
    log "Local 'master' is up to date with 'origin/main'."
else
    warn "neither 'origin/master' nor 'origin/main' exists — is 'origin' the
right remote? Continuing without a remote reference point; the safety
checks below will compare against local 'master' instead."
    BASE_REF="refs/heads/master"
fi

# ---------------------------------------------------------------------------
# 3. Deal with a pre-existing local 'main'
# ---------------------------------------------------------------------------

MAIN_DELETE_HINT="If you are sure the local 'main' can be discarded, delete
it yourself with:
    git branch -D main
and re-run this script."

if has_local_branch "main"; then
    log "Found an existing local branch 'main' (tip: $(git log -1 --format='%h %s' main))."
    echo >&2
    warn "'main' already exists locally. It will be DELETED so that the
renamed 'master' can take its place — but only if that cannot lose commits."
    echo >&2

    # Safety reference: local 'master'. The rename preserves all of
    # 'master', so deleting 'main' can only lose commits that are NOT
    # contained in 'master'. (Section 2 already verified that 'master'
    # is up to date with 'origin/main', so 'master' contains the whole
    # migrated remote history as well.)
    if is_ancestor "main" "master"; then
        log "'main' is fully contained in local 'master' (behind, equal,
or merged) — deleting it."
        git branch -D main
    else
        die "'main' has commits that are not in local 'master' (diverged
or ahead). Refusing to delete it, because that would lose them.
${MAIN_DELETE_HINT}"
    fi
else
    log "No existing local 'main' — good."
fi

# ---------------------------------------------------------------------------
# 4. Rename master -> main
# ---------------------------------------------------------------------------

log "Renaming branch 'master' to 'main' (git branch -m master main)..."
git branch -m master main
log "Renamed. This checkout is now on '$(current_branch)'."

if has_remote_branch "main"; then
    log "Setting upstream of 'main' to 'origin/main'..."
    git branch --set-upstream-to=origin/main main
else
    git branch --unset-upstream main ||
        true # no upstream configured, nothing to unset
fi

# ---------------------------------------------------------------------------
# 5. Clean up leftover refs
# ---------------------------------------------------------------------------

# After a successful rename no local 'master' ref can exist, but an
# aborted earlier run or manual meddling may have left one. Note: it
# cannot be checked out — we are on 'main' now.
if has_local_branch "master"; then
    log "Deleting leftover local branch 'master'..."
    git branch -D master
fi

log "Success. This checkout is now on 'main' and 'master' is gone locally."
log "Nothing was pushed — the remote is managed upstream."
