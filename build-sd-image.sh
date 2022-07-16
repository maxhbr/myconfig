#!/usr/bin/env bash
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

build() {
    hostname="$1"
    time nix build --out-link ../sd-image."${hostname}" --show-trace .#"${hostname}"-sd-image
}
./update.sh
build "${1:-pi4}"
