#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jhead

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
        jhead -autorot -nf%Y%m%d-%f "$img"
    fi
done
