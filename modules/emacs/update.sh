#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jq curl
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

cd "$( dirname "${BASH_SOURCE[0]}" )"
. ./../../lib/common.sh
updateRefAndJson syl20bnr/spacemacs
