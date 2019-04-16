#!/usr/bin/env nix-shell
#! nix-shell -i bash -p fgallery

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
