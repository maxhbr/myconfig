#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# prepare
../_hooks/30_dotfiles.sh
../_hooks/80_create_and_update_repos.pl

# run
set -ex
emacs --debug-init -nw --exec "(save-buffers-kill-emacs)"
