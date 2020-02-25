#!/usr/bin/env nix-shell
#! nix-shell -i bash -p xdg_utils
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

################################################################################
setupFilesAndSymlinks() {
    if [[ ! -f ~/.config/mimeapps.list ]]; then
        mkdir -p ~/.config
        touch ~/.config/mimeapps.list
    fi

    if [[ ! -L ~/.local/share/applications/mimeapps.list ]]; then
        if [[ -f ~/.local/share/applications/mimeapps.list ]]; then
            echo "remove old ~/.local/share/applications/mimeapps.list, content was:"
            cat ~/.local/share/applications/mimeapps.list
            rm ~/.local/share/applications/mimeapps.list
        fi
        mkdir -p ~/.local/share/applications
        ln -s ~/.config/mimeapps.list ~/.local/share/applications/mimeapps.list
    fi
}

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

################################################################################
logH1 "Setup xdg-mime"
setupFilesAndSymlinks
setupXdgMime application/pdf mupdf
setupXdgMime image/jpeg sxiv
setupXdgMime image/png sxiv
