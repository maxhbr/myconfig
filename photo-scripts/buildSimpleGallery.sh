#!/usr/bin/env nix-shell
#! nix-shell -i bash -p fgallery
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

if [[ "$1" == "--help" ]]; then
    cat<<EOF
  $0 img [img [img ...]]
EOF
    exit 0
fi

tmpdir=$(mktemp -d)
trap "{ rm -f $tmpdir; }" EXIT

cp $@ $tmpdir

fgallery $tmpdir html-gallery
