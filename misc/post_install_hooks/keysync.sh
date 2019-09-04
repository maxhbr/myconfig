#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

if [[ -e ~/.gnupg/keysync.sh ]]; then
    ~/.gnupg/keysync.sh || true
fi
