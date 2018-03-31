# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

bgs=("quint4.png"
     "penrose_4k_color.png"
     "quint3.png"
     "quint5.png"
     "quint7.png"
     "romben3.png"
     "romben.png")

LINK="$HOME/.background-image"

if [ ! -d $DIR ]; then
   echo '$DIR not well defined'
fi

getRandomBGFile() {
    rand=$[$RANDOM % ${#bgs[@]}]
    img="${bgs[$rand]}"

    (>&2 echo "\$1 is: $1")
    if [ "$1" ]; then
        if [ -d "$DIR/$1" ]; then
            (>&2 echo "yay, we made it")
            echo "$DIR/$1/${bgs[$rand]}"
            return
        fi
    fi
    echo "$DIR/${bgs[$rand]}"
}
