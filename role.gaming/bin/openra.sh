#!/usr/bin/env nix-shell
#! nix-shell -i bash -p appimage-run

url=https://github.com/OpenRA/OpenRA/releases/download/release-20200503/OpenRA-Red-Alert-x86_64.AppImage
out="$HOME/Downloads/OpenRA-Red-Alert-x86_64.AppImage"
wget \
    -O "$out" \
    -nc \
    https://github.com/OpenRA/OpenRA/releases/download/release-20200503/OpenRA-Red-Alert-x86_64.AppImage

appimage-run "$out"
