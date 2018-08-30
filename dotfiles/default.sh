#!/usr/bin/env nix-shell
#! nix-shell -i bash -p stow gitMinimal glibcLocales
# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

dotfiles="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
deploy() {
    echo "* $(tput bold)dotfiles$(tput sgr0) ..."

    if [ ! -d $HOME ]; then
        echo "user dir does not exist"
        exit 1
    fi

    cd "$dotfiles"

    add_stow_params="" # "--restow"
    if git diff-index --quiet HEAD --; then
        echo "git is clean, adopt files ..."
        add_stow_params="--adopt"
    fi

    user=$(stat -c '%U' $HOME)
    userGroup=$(stat -c '%G' $HOME)

    dirs=$(find . -mindepth 1 -maxdepth 1 -type d -not -name "_*" -exec basename {} \;)
    for dir in $dirs; do
        cd "$dotfiles/$dir"
        # I only want to have linked files, no linked folders
        find . -mindepth 1 -type d \
             -exec mkdir -p "$HOME/"{} \; \
             -exec chown $user:$userGroup "$HOME/"{} \;
        stow $add_stow_params -t $HOME -d $dotfiles $@ $dir
    done

    # create and update some repos in the user space
    $dotfiles/create_and_update_repos.pl
}

upgrade() {
    $dotfiles/xrdb_merge.sh
}


if [ $# -eq 0 ]; then
    deploy
    upgrade
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
