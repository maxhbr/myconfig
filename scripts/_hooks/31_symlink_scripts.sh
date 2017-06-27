#!/usr/bin/env bash

set -e
# link scripts ############################################################
echo "* symlink scripts to ~/bin"
mkdir -p ~/bin
find ./scripts -maxdepth 1 -executable -type f \
     -exec sh -c "ln -s \"$(pwd)/{}\" "'~/bin/$(basename {})' 2> /dev/null \;
