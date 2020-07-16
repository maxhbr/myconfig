#!/usr/bin/env bash
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

build() (
    local hostname="$1"
    local nixpkgsDir="$nixpkgs"

    set -x
    if [[ -z "$hostname" ]]; then
        local aarchConfig="${nixpkgsDir}/nixos/modules/installer/cd-dvd/sd-image-aarch64.nix"
    else
        local aarchConfig="${myconfigDir}/hosts/${hostname}/default.nix"
    fi
        time nix-build \
             -A config.system.build.sdImage \
             --option system aarch64-linux \
             -I nixos-config="${aarchConfig}" \
             -I nixpkgs="$nixpkgsDir" \
             --no-out-link \
             --show-trace --keep-failed \
             --option sandbox false \
             "${nixpkgsDir}/nixos/default.nix"
)

buildAndCopy() {
    local hostname="$1"
    drv=$(build "$hostname")
    out=("$drv/sd-image/"*".img")
    du -h "$out"
    if [[ -z "$hostname" ]]; then
        local outDir="$myconfigDir/__out/aarch64"
    else
        local outDir="$myconfigDir/__out/$hostname"
    fi
    install -D -m 644 -v "$out" -t "$outDir"
    nix-store --delete "$drv"

    cat <<EOF | tee "$outDir/dd.sh"
#!/usr/bin/env bash
set -ex
sdX="\$1"
sudo dd if="$outDir/$(basename "$out")" of="\$sdX" bs=4M conv=sync
EOF
    chmod +x "$outDir/dd.sh"
}

buildAndCopy "$1"
