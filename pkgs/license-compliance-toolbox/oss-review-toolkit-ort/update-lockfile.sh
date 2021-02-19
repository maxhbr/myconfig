#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nixFlakes jq git

set -euo pipefail

pushd ..
nix --experimental-features 'nix-command flakes' flake update --update-input ort
rev=$(cat flake.lock | jq -r '.nodes.ort.locked.rev')
popd
touch "$rev.fixedOutputSha256.json"
git add "$rev.fixedOutputSha256.json"
