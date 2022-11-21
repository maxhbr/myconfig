#!/usr/bin/env nix-shell
#! nix-shell -i bash -p restic pass

set -e

export RESTIC_PASSWORD="$(pass tng/restic)"
if [[ $1 = "init" ]]; then
    restic -r sftp:mhuber@nas:/mnt/tng-backup/tng-restic-repo init
fi
restic -r sftp:mhuber@nas:/mnt/tng-backup/tng-restic-repo --verbose backup ~/TNG

