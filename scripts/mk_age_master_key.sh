#!/usr/bin/env nix-shell
#! nix-shell -i bash -p age

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generate the master age key pair for the priv repo's agenix secrets
# (see scripts/secrets-encrypt.sh). Replaces the git-crypt "default" key
# as the human-held recovery key: every committed *.age file lists the
# master key as a recipient, so this one key can always re-provision a
# checkout via scripts/secrets-decrypt.sh.
#
# The private key must NEVER enter any repository. It is written to
# ~/myconfig/.age-master.key (override with AGE_MASTER_KEY);
# back it up offline. Losing it makes every *.age file undecryptable.

set -euo pipefail

key="${AGE_MASTER_KEY:-$HOME/myconfig/.age-master.key}"
pub="${key}.pub"

if [[ -e $key ]]; then
    echo "master key already exists at: $key"
    age-keygen -y "$key"
    exit 0
fi

mkdir -p "$(dirname "$key")"
umask 077
age-keygen -o "$key"
age-keygen -y "$key" >"$pub"
chmod 600 "$key"
chmod 644 "$pub"

echo "wrote private key: $key   (back this up offline!)"
echo "public key: $pub"
age-keygen -y "$key"
