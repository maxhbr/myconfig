#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gnupg git

set -ex
hostname="${1:?Usage: $0 <hostname>}"

# plaintext lives in the gitignored secrets-plain/ tree; the committed
# secrets/gpg/*.age ciphertext is produced by scripts/secrets-encrypt.sh
gpgDir="hosts/host.${hostname}/secrets-plain/gpg"
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
    "$(dirname "$0")/scripts/secrets-encrypt.sh"
else
    echo "already exists!"
fi
chmod 700 "$gpgDir"
GNUPGHOME="$gpgDir" gpg --fingerprint
GNUPGHOME="$gpgDir" gpg --list-secret-keys --keyid-format long
