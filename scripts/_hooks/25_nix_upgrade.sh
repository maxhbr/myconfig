#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# nix ######################################################################
if [ -d /etc/nix/ ]; then
    type "nix-env" &> /dev/null && {
        [ -x ./nix/upgrade.sh ] \
            && ./nix/upgrade.sh
    }
fi
