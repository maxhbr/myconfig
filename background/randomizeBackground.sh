#!/usr/bin/env bash
# Copyright  Maximilian Huber <oss@maximilian-huber.de>
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

getRandomBGFile() {
    rand=$[$RANDOM % ${#bgs[@]}]
    echo ${bgs[$rand]}
}

cd $(dirname $0)
rm background-image
ln -s $(getRandomBGFile) background-image

