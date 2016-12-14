#!/usr/bin/env bash

user=$(stat -c '%U' $0)
group=$(stat -c '%G' $0)

userDir="/home/$user"
if [ ! -d $userDir ]; then
    echo "user dir does not exist"
    exit 1
fi

dotfiles="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################
dirs=$(find . -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
for dir in $dirs; do
    cd "$dotfiles/$dir" \
        && find . -mindepth 1 -type d \
                -exec mkdir -p "$userDir/"{} \; \
                -exec chown $user:$group "$userDir/"{} \; \
        && stow -t $userDir -d $dotfiles $dir
done
