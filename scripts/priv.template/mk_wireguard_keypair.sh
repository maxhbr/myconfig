#!/usr/bin/env nix-shell
#! nix-shell -i bash -p wireguard-tools

set -ex
hostname="$1"

wgKeysDir="hosts/host.${hostname}/secrets-plain/wireguard-keys"
if [[ ! -d "$wgKeysDir" ]]; then
    mkdir -p "$wgKeysDir"
    wg genkey |
        tee "$wgKeysDir/private" |
        wg pubkey > "$wgKeysDir/public"
    "$(dirname "$0")/scripts/secrets-encrypt.sh"
else
    echo "already exists!"
fi
