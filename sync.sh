#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# sync.sh
#
# Sync this checkout and its beads with the gitolite remote:
#   1. fetch the remote and drop stale remote-tracking refs
#   2. fast-forward the local main to the remote main, or push local main
#      commits the remote lacks; a diverged main is refused, never rebased
#   3. `bd sync` the beads database (bd's own `sync.remote`, the same
#      gitolite repository, under refs/dolt/*)
#   4. prune the remote: delete branches that are fully merged into main
#
# Pruning only deletes refs/heads/* whose tip is reachable from the remote
# main, so no commit is lost. It never touches main, --keep branches, or
# branches checked out in a local worktree, and every deletion carries a
# --force-with-lease, so a branch pushed to since the fetch survives.
# Branches that are not merged (including squash-merged ones) are listed,
# never deleted.
#
# Usage:
#   ./sync.sh [options]
#
# Options:
#   -n, --dry-run      Fetch, then only print what would change locally and
#                      on the remote; skips `bd sync`
#   --no-beads         Skip `bd sync`
#   --no-prune         Skip deleting merged remote branches
#   --remote <name>    Git remote to sync with (default: gitolite)
#   --keep <branch>    Never delete this remote branch (repeatable)
#   -h, --help         Show this help
set -euo pipefail

remote=gitolite
branch=main
dry_run=false
do_beads=true
do_prune=true
keep=()

usage() {
    sed -n '/^# Usage:/,/^set -euo/{/^set -euo/d;s/^# \{0,1\}//;p}' "${BASH_SOURCE[0]}"
}

log() {
    printf '==> %s\n' "$*"
}

die() {
    printf 'sync.sh: %s\n' "$*" >&2
    exit 1
}

run() {
    if "$dry_run"; then
        printf '[dry-run]'
        printf ' %q' "$@"
        printf '\n'
    else
        "$@"
    fi
}

while (($#)); do
    case "$1" in
        -n | --dry-run) dry_run=true ;;
        --no-beads) do_beads=false ;;
        --no-prune) do_prune=false ;;
        --remote)
            (($# >= 2)) || die "--remote needs a value"
            remote=$2
            shift
            ;;
        --keep)
            (($# >= 2)) || die "--keep needs a value"
            keep+=("$2")
            shift
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        *)
            usage >&2
            exit 2
            ;;
    esac
    shift
done

cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" || exit 1

git remote get-url "$remote" >/dev/null 2>&1 || die "no git remote named '$remote'"

local_ref=refs/heads/$branch
remote_ref=refs/remotes/$remote/$branch
git show-ref --verify --quiet "$local_ref" || die "no local branch '$branch'"

# The worktree a branch is checked out in, if any.
worktree_of() {
    git worktree list --porcelain |
        awk -v want="branch refs/heads/$1" '
            /^worktree / { wt = substr($0, 10) }
            $0 == want { print wt }'
}

log "fetching $remote"
git fetch --prune "$remote"

log "syncing $branch with $remote/$branch"
local_sha=$(git rev-parse "$local_ref")
if ! git show-ref --verify --quiet "$remote_ref"; then
    log "$remote has no $branch yet, pushing it"
    run git push "$remote" "$local_ref:$local_ref"
else
    remote_sha=$(git rev-parse "$remote_ref")
    if [[ $local_sha == "$remote_sha" ]]; then
        log "$branch is up to date"
    elif git merge-base --is-ancestor "$local_sha" "$remote_sha"; then
        log "fast-forwarding $branch by $(git rev-list --count "$local_sha..$remote_sha") commit(s)"
        wt=$(worktree_of "$branch")
        if [[ -n $wt ]]; then
            run git -C "$wt" merge --ff-only --quiet "$remote_sha"
        else
            run git update-ref -m "sync.sh: fast-forward to $remote/$branch" \
                "$local_ref" "$remote_sha" "$local_sha"
        fi
    elif git merge-base --is-ancestor "$remote_sha" "$local_sha"; then
        log "pushing $(git rev-list --count "$remote_sha..$local_sha") commit(s) of $branch"
        run git push "$remote" "$local_ref:$local_ref"
    else
        die "$branch and $remote/$branch have diverged; merge or rebase by hand, then re-run"
    fi
fi

if "$do_beads"; then
    if ! command -v bd >/dev/null 2>&1; then
        log "bd not found, skipping beads"
    elif "$dry_run"; then
        log "[dry-run] would run: bd sync"
    else
        log "syncing beads"
        bd sync || {
            rc=$?
            die "bd sync failed with exit code $rc (see \`bd sync --help\` for its exit codes)"
        }
    fi
fi

if "$do_prune"; then
    log "pruning branches on $remote that are merged into $remote/$branch"
    mapfile -t checked_out < <(git worktree list --porcelain | sed -n 's|^branch refs/heads/||p')
    leases=()
    deletions=()
    while read -r name sha; do
        [[ $name == HEAD || $name == "$branch" ]] && continue
        if [[ " ${keep[*]} " == *" $name "* ]]; then
            log "keeping $name (--keep)"
            continue
        fi
        if [[ " ${checked_out[*]} " == *" $name "* ]]; then
            log "keeping $name (checked out in a local worktree)"
            continue
        fi
        log "deleting $name"
        leases+=("--force-with-lease=refs/heads/$name:$sha")
        deletions+=(":refs/heads/$name")
    done < <(git for-each-ref --merged "$remote_ref" \
        --format='%(refname:lstrip=3) %(objectname)' "refs/remotes/$remote/")

    if ((${#deletions[@]})); then
        run git push "${leases[@]}" "$remote" "${deletions[@]}"
    else
        log "no merged branches to delete"
    fi

    mapfile -t unmerged < <(git for-each-ref --no-merged "$remote_ref" \
        --format='%(refname:lstrip=3)' "refs/remotes/$remote/")
    if ((${#unmerged[@]})); then
        log "not merged into $branch, kept on $remote (${#unmerged[@]}): ${unmerged[*]}"
    fi
fi

log "done"
