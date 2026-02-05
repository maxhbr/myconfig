#!/usr/bin/env nix-shell
#! nix-shell -i bash -p syncthing

set -ex
hostname="$1"

syncthingDir="hosts/host.${hostname}/secrets/syncthing"
if [[ ! -f "$syncthingDir/key.pem" ]]; then
    syncthing -generate="${syncthingDir}"
    git add "$syncthingDir"
else
    echo "already exists!"
fi
