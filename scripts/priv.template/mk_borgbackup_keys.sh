#!/usr/bin/env nix-shell
#! nix-shell -i bash -p python39Packages.xkcdpass

set -euo pipefail
set -x
hostname="$1"

borgbackupDir="hosts/host.${hostname}/secrets/borgbackup"
if [[ ! -d "$borgbackupDir" ]]; then
  mkdir "$borgbackupDir"
  (
    cd "$borgbackupDir"
    ssh-keygen -f ssh_key -t ed25519 -C "Borg Backup"
    xkcdpass -n 12 > passphrase
  )
  git add "$borgbackupDir"
else
    echo "already exists!"
fi
