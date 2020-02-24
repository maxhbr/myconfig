#!/usr/bin/env nix-shell
#! nix-shell -i bash -p xdg_utils
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

logH1 "Setup xdg-mime"
setupXdgMime() {
    mimetype="$1"
    app="${2}.desktop"

    if [[ "$(xdg-mime query default "$mimetype")" != *"$app"* ]]; then
        logH3 "setup $app for $mimetype"
        set -x
        xdg-mime default "$app" "$mimetype"
        set +x
    fi
}

setupXdgMime application/pdf mupdf
setupXdgMime image/jpeg sxiv
setupXdgMime image/png sxiv
