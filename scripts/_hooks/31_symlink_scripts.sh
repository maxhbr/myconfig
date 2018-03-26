#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
# link scripts ############################################################
echo "* symlink scripts to ~/bin"
mkdir -p ~/bin
find ./scripts -maxdepth 1 -executable -type f \
     -exec sh -c "ln -s \"$(pwd)/{}\" "'~/bin/$(basename {})' 2> /dev/null \;
