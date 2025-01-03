#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

size=20G
file=/swapfile

if cat /proc/swaps | grep "^${file}\s"; then
    echo "The $file is already active"
    exit 0
fi

set -x

if [[ ! -f  $file ]]; then
    if mount | grep -q " on / type btrfs"; then
        sudo truncate -s 0 /swapfile
        sudo chattr +C /swapfile
        sudo btrfs property set /swapfile compression none
    fi
    sudo fallocate -l $size $file
    sudo chmod 600 $file
    sudo mkswap $file
fi
sudo swapon -f $file
