#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

if [ -e /etc/nixos/configuration.nix ]; then
    [ -x ./nixos/deploy.sh ] \
        && ./nixos/deploy.sh
fi
