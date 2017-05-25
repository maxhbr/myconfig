#!/usr/bin/env bash

# link dotfiles ###########################################################
[ -x ./dotfiles/deploy.sh ] \
    && ./dotfiles/deploy.sh
