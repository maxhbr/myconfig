#!/usr/bin/env bash

dotfiles="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

user=$(stat -c '%U' $0)
userGroup=$(stat -c '%G' $0)

userDir="/home/$user"
if [ ! -d $userDir ]; then
    echo "user dir does not exist"
    exit 1
fi

################################################################################
cd "$dotfiles"
dirs=$(find . -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
for dir in $dirs; do
    cd "$dotfiles/$dir"
    # I only want to have linked folders, no linked files
    find . -mindepth 1 -type d \
         -exec mkdir -p "$userDir/"{} \; \
         -exec chown $user:$userGroup "$userDir/"{} \;
    stow -t $userDir -d $dotfiles $dir
done
