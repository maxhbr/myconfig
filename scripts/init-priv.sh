#!/usr/bin/env nix-shell
#! nix-shell -i bash -p git
# shellcheck shell=bash

# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

cd "$(dirname "${BASH_SOURCE[0]}")/.."
set -e

myconfig="$(pwd)"
priv_template_dir="scripts/priv.template"
branch_name="priv.template"
use_test_dir=false
update_mode=false

for arg in "$@"; do
    if [[ $arg == "--test" ]]; then
        use_test_dir=true
    elif [[ $arg == "update" ]]; then
        update_mode=true
    elif [[ $update_mode == "true" ]]; then
        echo "Error: 'update' mode does not accept additional arguments" >&2
        exit 1
    fi
done

if ! git rev-parse --git-dir >/dev/null 2>&1; then
    echo "Error: Not in a git repository" >&2
    exit 1
fi

# Prefer the published priv.template branch (kept current by CI via
# scripts/update-priv.template-branch.sh); fall back to a local subtree
# split for repositories where the branch was never pushed.
template_source="branch"
if git show-ref --verify --quiet "refs/heads/$branch_name"; then
    :
elif [[ -n "$(git ls-remote origin "refs/heads/$branch_name" | cut -f1)" ]]; then
    git fetch origin "$branch_name:$branch_name"
else
    template_source="split"
fi

if [[ $template_source == "split" ]]; then
    split_branch="priv-subtree-branch"
    git branch -D "$split_branch" >/dev/null 2>&1 || true
    echo "Splitting subtree from $priv_template_dir..."
    git subtree split --prefix "$priv_template_dir" -b "$split_branch"
else
    split_branch="$branch_name"
fi

if [[ $update_mode == "true" ]]; then
    target_dir="../priv"
    if [[ ! -e $target_dir ]]; then
        echo "Error: '$target_dir' does not exist. Run init-priv.sh first to create it." >&2
        exit 1
    fi
    if [[ ! -d "$target_dir/.git" ]]; then
        echo "Error: '$target_dir' is not a git repository" >&2
        exit 1
    fi
    if [[ $use_test_dir == "true" ]]; then
        echo "Error: 'update' mode does not support --test flag" >&2
        exit 1
    fi

    timestamp=$(date +%Y%m%d%H%M%S)
    backup_dir="../priv.$timestamp"
    echo "Creating backup at: $backup_dir"
    cp -a "$target_dir" "$backup_dir"
    echo "Backup created successfully"

    cd "$target_dir"
    echo "Updating private config repository from $split_branch..."
    git pull --no-ff "$myconfig" "$split_branch"
    cd "$myconfig"

    if [[ $template_source == "split" ]]; then
        git branch -D "$split_branch" >/dev/null 2>&1
    fi

    echo "Updated private config repository at: $target_dir"
else
    if [[ $use_test_dir == "true" ]]; then
        target_dir="$(mktemp -d -t myconfig-priv-XXXXXX)"
    else
        target_dir="../priv"
        if [[ -e $target_dir ]]; then
            echo "Error: Target '$target_dir' already exists. Use 'update' to update existing repository." >&2
            exit 1
        fi
    fi

    mkdir -p "$target_dir"
    cd "$target_dir"
    git init

    echo "Importing template history..."
    git pull -q "$myconfig" "$split_branch"

    cd "$myconfig"

    if [[ $template_source == "split" ]]; then
        git branch -D "$split_branch" >/dev/null 2>&1
    fi

    if [[ $use_test_dir == "true" ]]; then
        echo "Test repo created at: $target_dir"
    else
        echo "Initialized private config repository at: $target_dir"
    fi
fi
