#!/usr/bin/env bash

set -e

echo "* $(tput bold)xmonad$(tput sgr0) ..."

src="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
target="$HOME/.xmonad"

mkdir -p "$target"

declare -a folders=(lib bin neo)
for folder in ${folders[@]}; do
    [[ -e "$target/$folder" ]] || ln -s "$src/$folder" "$target/$folder"
done

declare -a files=(xmonad.hs xmobarrc)
for file in ${files[@]}; do
    [[ -e "$target/$file" ]] || ln -s "$src/$file" "$target/$file"
done

if [ ! -z ${DISPLAY+x} ]; then
    xmonad --recompile
    sleep 0.1
    xmonad --restart
fi

notification_pipe="$target/notification.pipe"
[[ -p $notification_pipe ]] || mkfifo $notification_pipe
