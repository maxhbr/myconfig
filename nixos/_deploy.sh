#!/usr/bin/env bash
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
config=/etc/nixos/configuration.nix
echo "* $(tput bold)generate $config$(tput sgr0) ..."
if [[ ! -f $config ]] || [[ $(wc -l <$config) -eq 1 ]]; then
    echo "import $SRC" | sudo tee $config
else
    echo "$config contains unexpected content"
    exit 1
fi
