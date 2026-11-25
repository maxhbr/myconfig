#!/usr/bin/env nix-shell
#! nix-shell -i bash -p openssh git

set -ex
hostname="$1"

# plaintext lands in the gitignored secrets-plain/ tree; the committed
# secrets/ssh/*.age ciphertext is produced by scripts/secrets-encrypt.sh
sshDir="hosts/host.${hostname}/secrets-plain/ssh"
if [[ ! -d "$sshDir" ]]; then
    mkdir -p "$sshDir"
    ssh-keygen -f "$sshDir/id_ed25519" -t ed25519 -C "${hostname}"
    "$(dirname "$0")/scripts/secrets-encrypt.sh"
else
    echo "already exists!"
fi