#!/usr/bin/env bash

# link dotfiles ###########################################################
echo "* dotfiles ..."
[ -x ./dotfiles/deploy.sh ] \
    && ./dotfiles/deploy.sh
