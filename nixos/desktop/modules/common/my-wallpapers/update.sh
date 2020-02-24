#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jq curl
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

cd "$( dirname "${BASH_SOURCE[0]}" )"
. ./../../../../../common.sh
updateRefAndJson maxhbr/wallpapers master maxhbr-wallpapers
