#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

handleChannel() {
    local channel=$1
    logH3 "update" "$channel"
    local releaseUrl="$(curl -Ls -o /dev/null -w %{url_effective} "https://channels.nixos.org/${channel}")"
    local rev=$(curl -L -s "$releaseUrl/git-revision")
    updateRefAndJson NixOS/nixpkgs $rev $channel
}

# TODO: needs remote (already there) and fetch
run() {
    local wasUpdated=0

    handleChannel nixos-unstable || wasUpdated=1
    handleChannel nixos-unstable-small || wasUpdated=1
    handleChannel nixpkgs-unstable || wasUpdated=1
    handleChannel nixos-20.03 || wasUpdated=1
    handleChannel nixos-20.03-small || wasUpdated=1

    return $wasUpdated
}

run
