#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jq curl
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

updateRefAndJson vlaci/nix-doom-emacs master
