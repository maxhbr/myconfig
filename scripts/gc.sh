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

before=$(logUsage)

echo "* nix-env --delete-generations $age ..."
nix-env --delete-generations $age
echo "* sudo nix-env --delete-generations $age ..."
sudo nix-env --delete-generations $age || echo "failed, probably when waiting for sudo PW"
echo "* sudo nix-collect-garbage --delete-older-than $age ..."
sudo nix-collect-garbage \
     --delete-older-than $age || echo "failed, probably when waiting for sudo PW"

##  haskellPackages.stack-clean-old
# echo "* cleanup stack"
# stack-clean-old keep-minor -S -o x86_64-linux-nix -d

after=$(logUsage)

echo "* difference in usage:"
{ set +e;
  diff -y "$before" "$after";
  rm "$before" "$after"
}
