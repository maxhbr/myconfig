#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

type "nix-collect-garbage" &> /dev/null && {
    if [ "$((RANDOM%100))" -gt 90 ]; then
        echo "* nix-collect-garbage --delete-generations 30d ..."
        nix-collect-garbage --delete-older-than 30d
        sudo nix-collect-garbage --delete-older-than 30d
        echo "** kept generations are:"
        sudo nix-env -p /nix/var/nix/profiles/system --list-generations
        echo "** number of generations in the boot loader: $(ls /boot/loader/entries | wc -l)"
    else
        echo "* $(tput bold)do not$(tput sgr0) nix-collect-garbage --delete-generations 30d"
    fi
}
