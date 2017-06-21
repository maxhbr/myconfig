#!/usr/bin/env bash
type "nix-collect-garbage" &> /dev/null && {
    echo "* nix-collect-garbage --delete-generations 30d ..."
    nix-collect-garbage --delete-older-than 30d
    sudo nix-collect-garbage --delete-older-than 30d
}
