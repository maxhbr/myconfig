#!/usr/bin/env nix-shell
#! nix-shell -i bash -p wireguard-tools

set -ex
hostname="$1"

wgKeysDir="hosts/host.${hostname}/secrets/wireguard-keys"
if [[ ! -d "$wgKeysDir" ]]; then
    mkdir -p "$wgKeysDir"
    wg genkey |
        tee "$wgKeysDir/private" |
        wg pubkey > "$wgKeysDir/public"
    git add "$wgKeysDir"
else
    echo "already exists!"
fi
