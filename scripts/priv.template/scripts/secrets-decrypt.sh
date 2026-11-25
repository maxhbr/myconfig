#!/usr/bin/env nix-shell
#! nix-shell -i bash -p age

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Provisioning: decrypt every committed **/secrets/*.age into the
# parallel gitignored **/secrets-plain/ tree, which is what
# `myconfig.secrets.<name>.source`, builtins.readFile consumers and
# the eval machine read. Run from the priv repo root on a fresh
# checkout (after cloning, before evaluating the flake).
#
# Decryption uses the master key (AGE_MASTER_KEY, default
# ~/myconfig/.age-master.key). On the target hosts themselves,
# agenix decrypts at activation via age.identityPaths — this script is
# only for the *eval/provisioning* checkout.
#
# Idempotent: skips files whose plaintext is already up to date.

set -euo pipefail

repo="$(git rev-parse --show-toplevel)"
cd "$repo"

key="${AGE_MASTER_KEY:-$HOME/myconfig/.age-master.key}"
if [[ ! -f $key ]]; then
    echo "ERROR: no master age key at $key" >&2
    echo "generate one first: scripts/mk_age_master_key.sh" >&2
    exit 1
fi

count=0
skipped=0
while IFS= read -r -d '' f; do
    rel="${f#./}"
    if [[ $rel == secrets/* ]]; then
        plain="secrets-plain/${rel#secrets/}"
    else
        plain="${rel/secrets/secrets-plain}"
    fi
    plain="${plain%.age}"

    if [[ -e $plain && $f -ot $plain && ${SECRETS_FORCE:-} != 1 ]]; then
        skipped=$((skipped + 1))
        continue
    fi

    mkdir -p "$(dirname "$plain")"
    age -d -i "$key" -o "$plain" "$rel"
    chmod 600 "$plain"
    count=$((count + 1))
    echo "decrypted: $rel -> $plain"
done < <(find . -path ./.git -prune -o -type f -name '*.age' \( -path '*/secrets/*' -o -path 'secrets/*' \) -print0)

echo "decrypted $count file(s), skipped $skipped up-to-date file(s)."
if ((count == 0 && skipped == 0)); then
    echo "WARN: no *.age files found under secrets/ or hosts/*/secrets/" >&2
fi
