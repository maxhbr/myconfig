#!/usr/bin/env bash

# prepare
../_hooks/30_dotfiles.sh
../_hooks/80_create_and_update_repos.pl

# run
set -ex
emacs --debug-init -nw --exec "(save-buffers-kill-emacs)"
