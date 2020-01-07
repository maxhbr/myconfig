#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jq curl
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

cd "$( dirname "${BASH_SOURCE[0]}" )"
. "$(pwd )/../../common.sh"

set -e

branch=release-19.09

update() {
    logH3 "update home-manager on branch" "$channel"
    local rev="$(curl -s "https://api.github.com/repos/rycee/home-manager/commits/${branch}" | jq -r '.sha')"

    if ! grep -q $rev "./home-manager.rev" 2>/dev/null; then
        echo $rev > "./home-manager.rev"

        local tarball="https://github.com/rycee/home-manager/archive/${rev}.tar.gz"
        prefetchOutput=$(nix-prefetch-url --unpack --print-path --type sha256 $tarball)
        local hash=$(echo "$prefetchOutput" | head -1)
        local path=$(echo "$prefetchOutput" | tail -1)
        echo '{"url":"'$tarball'","rev": "'$rev'","sha256":"'$hash'","path":"'$path'","ref": "'$branch'"}' > "./home-manager.json"
        logINFO "... updated ./home-manager.rev to rev=[$rev]"
    else
        logINFO "... ./home-manager.rev file is already up to date, at rev=[$rev]"
    fi
}

update
