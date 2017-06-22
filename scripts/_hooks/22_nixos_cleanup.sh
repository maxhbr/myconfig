#!/usr/bin/env bash
type "nix-collect-garbage" &> /dev/null && {
    echo "* nix-collect-garbage --delete-generations 30d ..."
    nix-collect-garbage --delete-older-than 30d
    sudo nix-collect-garbage --delete-older-than 30d
    echo "** kept generations are:"
    sudo nix-env -p /nix/var/nix/profiles/system --list-generations
    echo "** number of generations in the boot loader: $(ls /boot/loader/entries | wc -l)"
}
