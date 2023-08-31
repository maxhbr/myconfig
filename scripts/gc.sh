#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget tmux glibcLocales

# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

cd "$( dirname "${BASH_SOURCE[0]}" )"
set -e

age=${1:-30d}

logUsage() {
    o=$(mktemp)
    df -h --output=file,used,pcent /nix/store /boot > "$o"
    echo "$o"
}

read -p "also clean /nix/var/nix/gcroots/auto/? " -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]; then
    for link in /nix/var/nix/gcroots/auto/* ;do
        rl="$(readlink "$link")"
        echo "* rm $rl"
        sudo rm "$rl" || true
    done
fi

before=$(logUsage)

echo "* nix-store --gc ..."
sudo nix-store --gc

echo "* nix-env --delete-generations $age ..."
nix-env --delete-generations $age

echo "* sudo nix-env --delete-generations $age ..."
sudo nix-env --delete-generations $age || echo "failed, probably when waiting for sudo PW"

echo "* sudo nix-collect-garbage --delete-older-than $age ..."
sudo nix-collect-garbage \
     --delete-older-than $age || echo "failed, probably when waiting for sudo PW"

after=$(logUsage)

echo "* difference in usage:"
{ set +e;
  diff -y "$before" "$after";
  rm "$before" "$after"
}
