#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jq curl
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

cd "$( dirname "${BASH_SOURCE[0]}" )"
. ./../../lib/common.sh
repo="syl20bnr/spacemacs"
outName="$(basename "$repo")"
outRev="${outName}.rev"
outJson="${outName}.json"
branch=master

update() {
    logH3 "update $outName on branch" "$branch"
    local rev="$(curl -s "https://api.github.com/repos/$repo/commits/${branch}" | jq -r '.sha')"

    if ! grep -q $rev "$outRev" 2>/dev/null; then
        echo $rev > "$outRev"

        local tarball="https://github.com/$repo/archive/${rev}.tar.gz"
        prefetchOutput=$(nix-prefetch-url --unpack --print-path --type sha256 $tarball)
        local hash=$(echo "$prefetchOutput" | head -1)
        local path=$(echo "$prefetchOutput" | tail -1)
        echo '{"url":"'$tarball'","rev": "'$rev'","sha256":"'$hash'","path":"'$path'","ref": "'$branch'"}' > "./$outJson"
        logINFO "... updated $outRev to rev=[$rev]"
    else
        logINFO "... $outRev file is already up to date, at rev=[$rev]"
    fi
}

update
