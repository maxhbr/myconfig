#!/usr/bin/env nix-shell
#! nix-shell -i bash -p appimage-run

url=https://github.com/OpenRA/OpenRA/releases/download/release-20200503/OpenRA-Dune-2000-x86_64.AppImage
out="$HOME/Downloads/OpenRA-Dune-2000-x86_64.AppImage"
wget \
    -O "$out" \
    -nc \
    $url

appimage-run "$out"
