# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# default: list available recipes
default:
    @just --list

# Runs `nvfetcher` to regenerate `_sources/generated.{nix,json}` from
# `nvfetcher.toml` (the weekly CI counterpart is
# .github/workflows/update-sources.yml).
# Review the git diff afterwards — a changed rev/hash is the point of the run.
[linux]
[macos]
sources:
    nix run nixpkgs#nvfetcher

# Validate a `test-<hostname>` NixOS configuration of this flake without
# modifying the system (runs `nix flake check` + a toplevel build).
# Takes a short host name (without the `test-` prefix); defaults to the
# current host. Nothing on the running system is changed.
[linux]
[macos]
test host='':
    ./scripts/test.sh {{host}}

remote := 'gitolite'
branch := 'main'

# Add an ssh key to the agent (prefers the passphrase-less one)
[private]
_add-key:
    #!/usr/bin/env bash
    set -euo pipefail
    key=$HOME/.ssh/id_ed25519_no_pw
    [[ -f $key ]] || key=$HOME/.ssh/id_ed25519
    if [[ -f $key ]]; then
        echo "==> adding ${key##*/} to ssh-agent"
        ssh-add "$key" >/dev/null 2>&1 || true
    fi

# Sync local main with the gitolite remote, then run `bd sync`
[linux]
[macos]
sync: _add-key
    #!/usr/bin/env bash
    set -euo pipefail
    remote={{remote}}
    branch={{branch}}
    git remote get-url "$remote" >/dev/null 2>&1 || { echo "no git remote named '$remote'" >&2; exit 1; }

    echo "==> fetching $remote"
    git fetch --prune "$remote"

    echo "==> syncing $branch with $remote/$branch"
    local_ref=refs/heads/$branch
    remote_ref=refs/remotes/$remote/$branch
    local_sha=$(git rev-parse "$local_ref")
    if ! git show-ref --verify --quiet "$remote_ref"; then
        echo "$remote has no $branch yet, pushing it"
        git push "$remote" "$local_ref:$local_ref"
    else
        remote_sha=$(git rev-parse "$remote_ref")
        if [[ $local_sha == "$remote_sha" ]]; then
            echo "$branch is up to date"
        elif git merge-base --is-ancestor "$local_sha" "$remote_sha"; then
            echo "fast-forwarding $branch by $(git rev-list --count "$local_sha..$remote_sha") commit(s)"
            wt=$(git worktree list --porcelain | \
                awk -v want="branch refs/heads/$branch" '
                    /^worktree / { wt = substr($0, 10) }
                    $0 == want { print wt }')
            if [[ -n $wt ]]; then
                git -C "$wt" merge --ff-only --quiet "$remote_sha"
            else
                git update-ref -m "sync: fast-forward to $remote/$branch" \
                    "$local_ref" "$remote_sha" "$local_sha"
            fi
        elif git merge-base --is-ancestor "$remote_sha" "$local_sha"; then
            echo "pushing $(git rev-list --count "$remote_sha..$local_sha") commit(s) of $branch"
            git push "$remote" "$local_ref:$local_ref"
        else
            echo "$branch and $remote/$branch have diverged; merge or rebase by hand, then re-run" >&2
            exit 1
        fi
    fi

    if command -v bd >/dev/null 2>&1; then
        echo "==> syncing beads"
        bd sync
    else
        echo "==> bd not found, skipping beads"
    fi

# Dry-run sync: fetch and print what would change, skip `bd sync`
[linux]
[macos]
sync-dry-run: _add-key
    #!/usr/bin/env bash
    set -euo pipefail
    remote={{remote}}
    branch={{branch}}
    git remote get-url "$remote" >/dev/null 2>&1 || { echo "no git remote named '$remote'" >&2; exit 1; }

    echo "==> fetching $remote"
    git fetch --prune "$remote"

    local_ref=refs/heads/$branch
    remote_ref=refs/remotes/$remote/$branch
    local_sha=$(git rev-parse "$local_ref")
    if ! git show-ref --verify --quiet "$remote_ref"; then
        echo "[dry-run] $remote has no $branch yet, would push it"
    else
        remote_sha=$(git rev-parse "$remote_ref")
        if [[ $local_sha == "$remote_sha" ]]; then
            echo "$branch is up to date"
        elif git merge-base --is-ancestor "$local_sha" "$remote_sha"; then
            echo "[dry-run] would fast-forward $branch by $(git rev-list --count "$local_sha..$remote_sha") commit(s)"
        elif git merge-base --is-ancestor "$remote_sha" "$local_sha"; then
            echo "[dry-run] would push $(git rev-list --count "$remote_sha..$local_sha") commit(s) of $branch"
        else
            echo "$branch and $remote/$branch have diverged; merge or rebase by hand, then re-run" >&2
            exit 1
        fi
    fi
    echo "[dry-run] would run: bd sync"

# Prune branches merged into main from ALL remotes
[linux]
[macos]
prune: _add-key
    #!/usr/bin/env bash
    set -euo pipefail
    branch={{branch}}
    mapfile -t checked_out < <(git worktree list --porcelain | sed -n 's|^branch refs/heads/||p')

    while read -r remote; do
        echo "==> fetching $remote"
        git fetch --prune "$remote"

        remote_ref=refs/remotes/$remote/$branch
        git show-ref --verify --quiet "$remote_ref" || { echo "==> $remote has no $branch, skipping"; continue; }

        echo "==> pruning branches on $remote merged into $remote/$branch"
        leases=()
        deletions=()
        while read -r name sha; do
            [[ $name == HEAD || $name == "$branch" ]] && continue
            if [[ " ${checked_out[*]} " == *" $name "* ]]; then
                echo "keeping $name (checked out in a local worktree)"
                continue
            fi
            echo "deleting $name"
            leases+=("--force-with-lease=refs/heads/$name:$sha")
            deletions+=(":refs/heads/$name")
        done < <(git for-each-ref --merged "$remote_ref" \
            --format='%(refname:lstrip=3) %(objectname)' "refs/remotes/$remote/")

        if ((${#deletions[@]})); then
            git push "${leases[@]}" "$remote" "${deletions[@]}"
        else
            echo "no merged branches to delete on $remote"
        fi

        mapfile -t unmerged < <(git for-each-ref --no-merged "$remote_ref" \
            --format='%(refname:lstrip=3)' "refs/remotes/$remote/")
        if ((${#unmerged[@]})); then
            echo "not merged into $branch, kept on $remote (${#unmerged[@]}): ${unmerged[*]}"
        fi
    done < <(git remote)
