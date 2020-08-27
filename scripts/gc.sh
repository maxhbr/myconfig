#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget tmux glibcLocales
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"
set -e

age=${1:-30d}

logUsage() {
    o=$(mktemp)
    df -h --output=file,used,pcent /nix/store > "$o"
    echo $o
}

before=$(logUsage)

echo "* nix-env --delete-generations $age ..."
nix-env $NIX_PATH_ARGS \
        --delete-generations $age
echo "* sudo nix-env --delete-generations $age ..."
sudo nix-env $NIX_PATH_ARGS \
             --delete-generations $age || echo "failed, probably when waiting for sudo PW"
echo "* sudo nix-collect-garbage --delete-older-than $age ..."
sudo nix-collect-garbage \
     --delete-older-than $age || echo "failed, probably when waiting for sudo PW"

after=$(logUsage)

echo "* difference in usage:"
{ set +e;
  diff -y "$before" "$after";
  rm "$before" "$after"
}
