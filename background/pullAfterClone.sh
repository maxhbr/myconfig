#!/usr/bin/env nix-shell
#! nix-shell -i bash -p git git-lfs
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -ex

git lfs install
git lfs fetch
git lfs checkout
