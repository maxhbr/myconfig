#!/usr/bin/env nix-shell
#! nix-shell -i bash -p wireguard-tools pass
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -ex

if [[ "$1" == "-h" ]]; then
    cat <<EOF
   $ $0 NAME_OF_DEVICE
EOF
fi

name="$1"
if [[ -z "$name" ]]; then
    echo "\$name should not be empty"
    exit 1
fi

tmp="$(mktemp -d)"
cd "$tmp"
trap 'cd - && rm -rf $tmp' EXIT
wg genkey | tee privatekey | wg pubkey > publickey

cat <<EOF >out
$(cat privatekey)

pubkey: $(cat publickey)
EOF

pass insert -fm "wireguard/$name" < out
rm out
