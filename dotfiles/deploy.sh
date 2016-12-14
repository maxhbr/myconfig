#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################
if [ $1 ]; then
    user=$1
else
    user=$(awk -F: "/:1000:/{print \$1}" /etc/passwd)
fi
userDir="/home/$user"

if [ ! -d $userDir ]; then
    echo "user dir does not exist"
    exit 1
fi

dotfiles="$DIR"

################################################################################
prepareFolders() {
    dirs="bin .config .emacs .vim"
    for dir in $dirs; do
        mkdir -p "$userDir/$dir"
        chown 1000:1000 "$userDir/$dir"
    done
}


################################################################################
stowConfigs() {
    dirs=$(find . -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
    for dir in $dirs; do
        stow -t $userDir -d $dotfiles $dir
    done
}

prepareFolders
stowConfigs
