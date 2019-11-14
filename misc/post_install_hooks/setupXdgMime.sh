#!/usr/bin/env nix-shell
#! nix-shell -i bash -p xdg_utils
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

setupXdgMime() {
    mimetype="$1"
    app="${2}.desktop"

    echo "setup $app for $mimetype"
    if [[ "$(xdg-mime query default "$mimetype")" != *"$app"* ]]; then
        set -x
        xdg-mime default "$app" "$mimetype"
        set +x
    else
        echo "... already done"
    fi
}

setupXdgMime application/pdf mupdf
setupXdgMime image/jpeg sxiv
setupXdgMime image/png sxiv
