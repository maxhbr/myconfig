#!/usr/bin/env nix-shell
#! nix-shell -i bash -p syncthing

set -ex
hostname="$1"

syncthingDir="hosts/host.${hostname}/secrets-plain/syncthing"
if [[ ! -f "$syncthingDir/key.pem" ]]; then
    syncthing -generate="${syncthingDir}"
    "$(dirname "$0")/scripts/secrets-encrypt.sh"
else
    echo "already exists!"
fi
