#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

# nix ######################################################################
if [ -d /etc/nix/ ]; then
    [ -x ./nix/deploy.sh ] \
        && ./nix/deploy.sh
fi
