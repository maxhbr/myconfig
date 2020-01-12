#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
. "$(pwd )/../../common.sh"

handleChannel() {
    local channel=$1
    logH3 "update" "$channel"
    local rev=$(curl -L -s "https://nixos.org/channels/${channel}/git-revision")
    updateRefAndJson NixOS/nixpkgs $rev $channel
}

# TODO: needs remote (already there) and fetch
handleChannel nixos-unstable
handleChannel nixos-unstable-small
handleChannel nixpkgs-unstable
