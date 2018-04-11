# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

bgs=("quint4.png"
     "penrose_4k_color.png"
     "quint3.png"
     "quint5.png"
     "quint7.png"
     "romben3.png"
     "romben.png")

rand=$[$RANDOM % ${#bgs[@]}]
img="${bgs[$rand]}"

if [ "$1" ]; then
    if [ -d "$DIR/$1" ]; then
        echo "$DIR/$1/${bgs[$rand]}"
        return
    fi
fi
echo "$DIR/${bgs[$rand]}"
