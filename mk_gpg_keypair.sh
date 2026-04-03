#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gnupg git

set -ex
hostname="${1:?Usage: $0 <hostname>}"

gpgDir="hosts/host.${hostname}/secrets/gpg"
if [[ ! -d "$gpgDir" ]]; then
    mkdir -p "$gpgDir"
    chmod 700 "$gpgDir"
    GNUPGHOME="$gpgDir" gpg --batch --gen-key <<EOF
%no-protection
Key-Type: eddsa
Key-Curve: ed25519
Subkey-Type: ecdh
Subkey-Curve: cv25519
Name-Real: ${hostname}
Name-Email: ${hostname}@localhost
Expire-Date: 0
%commit
EOF
    GNUPGHOME="$gpgDir" gpg --armor --export > "$gpgDir/public.asc"
    GNUPGHOME="$gpgDir" gpg --armor --export-secret-keys > "$gpgDir/private.asc"
    git add "$gpgDir"
else
    echo "already exists!"
fi
chmod 700 "$gpgDir"
GNUPGHOME="$gpgDir" gpg --fingerprint
GNUPGHOME="$gpgDir" gpg --list-secret-keys --keyid-format long
