#!/usr/bin/env nix-shell
#! nix-shell -i bash -p imagemagick
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

untiffFile() {
    local file=$1
    local pngFile="${file%.*}.png"

    if [[ -f "$pngFile" ]]; then
        echo "ERROR: the file $pngFile already exists!"
    else
        convert "$file" "$pngFile"
    fi
}

for file in "$@"; do
    untiffFile "$file"
done
