#!/usr/bin/env nix-shell
#! nix-shell -i bash -p openssh git

set -ex
hostname="$1"

sshDir="hosts/host.${hostname}/secrets/ssh"
if [[ ! -d "$sshDir" ]]; then
    mkdir -p "$sshDir"
    ssh-keygen -f "$sshDir/id_ed25519" -t ed25519 -C "${hostname}"
    git add "$sshDir"
else
    echo "already exists!"
fi