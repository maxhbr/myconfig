#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

# link xmonad #############################################################
[ -x ./xmonad/deploy.sh ] \
    && ./xmonad/deploy.sh
