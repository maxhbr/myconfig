#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget tmux glibcLocales
# shellcheck shell=bash

# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

cd "$(dirname "${BASH_SOURCE[0]}")"
set -e

age=${1:-30d}

SUDO_KEEPALIVE_PID=""
sudo_keepalive_stop() {
    if [[ -n $SUDO_KEEPALIVE_PID ]]; then
        kill "$SUDO_KEEPALIVE_PID" 2>/dev/null || true
        SUDO_KEEPALIVE_PID=""
    fi
}
# Ask for the sudo password up front and keep the timestamp alive until
# the script exits: the gc can run long enough for the timestamp to
# expire, and a password prompt in the middle of it fails the cleanup.
sudo_keepalive_start() {
    if [[ $EUID -eq 0 || -n $SUDO_KEEPALIVE_PID ]]; then
        return 0
    fi
    echo "* priming sudo credentials ..."
    if ! sudo -v; then
        echo "could not obtain sudo credentials, continuing without keep-alive"
        return 0
    fi
    local parent=$$
    while kill -0 "$parent" 2>/dev/null; do
        sudo -n -v 2>/dev/null || break
        sleep 50
    done &
    SUDO_KEEPALIVE_PID=$!
    trap sudo_keepalive_stop EXIT
}

sudo_keepalive_start

logUsage() {
    o=$(mktemp)
    df -h --output=file,used,pcent /nix/store /boot >"$o"
    echo "$o"
}

read -p "also clean /nix/var/nix/gcroots/auto/? " -n 1 -r
echo # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]; then
    for link in /nix/var/nix/gcroots/auto/*; do
        rl="$(readlink "$link")"
        echo "* rm $rl"
        sudo rm "$rl" || true
    done
fi

before=$(logUsage)

echo "* nix-store --gc ..."
sudo nix-store --gc

echo "* nix-env --delete-generations $age ..."
nix-env --delete-generations "$age"

echo "* sudo nix-env --delete-generations $age ..."
sudo nix-env --delete-generations "$age" || echo "failed, probably when waiting for sudo PW"

echo "* sudo nix-collect-garbage --delete-older-than $age ..."
sudo nix-collect-garbage \
    --delete-older-than "$age" || echo "failed, probably when waiting for sudo PW"

after=$(logUsage)

echo "* difference in usage:"
{
    set +e
    diff -y "$before" "$after"
    rm "$before" "$after"
}
