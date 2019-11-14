#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

setup() {
    mimetype="$1"
    app="${2}.desktop"

    if [[ "$(xdg-mime query default application/pdf)" != *"$app"* ]]; then
        set -x
        xdg-mime default "$app" application/pdf
        set +x
    fi
}

setup "application/pdf" mupdf
