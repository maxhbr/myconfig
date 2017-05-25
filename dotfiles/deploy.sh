#!/usr/bin/env bash

set -e

echo "* $(tput bold)dotfiles$(tput sgr0) ..."

dotfiles="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

user=$(stat -c '%U' $0)
userGroup=$(stat -c '%G' $0)

if [ ! -d $HOME ]; then
    echo "user dir does not exist"
    exit 1
fi

################################################################################
cd "$dotfiles"

add_stow_params=""
if git diff-index --quiet HEAD --; then
    echo "git is clean, adopt files ..."
    add_stow_params="--adopt"
fi

dirs=$(find . -mindepth 1 -maxdepth 1 -type d -not -name "_*" -exec basename {} \;)
for dir in $dirs; do
    cd "$dotfiles/$dir"
    # I only want to have linked files, no linked folders
    find . -mindepth 1 -type d \
         -exec mkdir -p "$HOME/"{} \; \
         -exec chown $user:$userGroup "$HOME/"{} \;
    stow $add_stow_params -t $HOME -d $dotfiles $@ $dir
done
