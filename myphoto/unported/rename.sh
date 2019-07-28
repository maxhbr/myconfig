#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jhead
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# TODO: implement for png / tif / ARW / ...

set -e

if [[ "$1" == "--help" ]]; then
    cat<<EOF
  $0 img [img [img ...]]
EOF
    exit 0
fi

for img in "$@"; do
    if [[ -f "$img" ]]; then
        local extension="$(echo "${exifImage##*.}" | tr '[:upper:]' '[:lower:]')"
        if [[ "$extension" == "jpg" ]]; then
            jhead -autorot -nf%Y%m%d-%f "$img"
        else
            echo "unsuported file: $img"
        fi
    fi
done
