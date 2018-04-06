#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

# link dotfiles ###########################################################
[ -x ./dotfiles/deploy.sh ] \
    && ./dotfiles/deploy.sh
