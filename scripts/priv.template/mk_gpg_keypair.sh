#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gnupg git

set -ex
hostname="$1"

gpgDir="hosts/host.${hostname}/secrets/gpg"
if [[ ! -d "$gpgDir" ]]; then
    mkdir -p "$gpgDir"
    GNUPGHOME="$gpgDir" gpg --batch --gen-key <<EOF
%no-protection
Key-Type: ed25519
Key-Usage: sign
Subkey-Type: cv25519
Subkey-Usage: encrypt
Name-Real: ${hostname}
Name-Email: ${hostname}@localhost
Expire-Date: 0
EOF
    GNUPGHOME="$gpgDir" gpg --armor --export > "$gpgDir/public.asc"
    git add "$gpgDir"
else
    echo "already exists!"
fi
