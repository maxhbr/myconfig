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
        # see: https://stackoverflow.com/questions/27267073/imagemagick-lossless-max-compression-for-png
        convert -depth 24 \
                -define png:compression-filter=2 \
                -define png:compression-level=9 \
                -define png:compression-strategy=1 \
                "$file" "$pngFile"
    fi
}

for file in "$@"; do
    untiffFile "$file"
done
