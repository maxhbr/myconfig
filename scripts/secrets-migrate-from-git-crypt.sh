#!/usr/bin/env nix-shell
#! nix-shell -i bash -p age jq git

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# One-shot bootstrap of the git-crypt -> age/agenix migration in the
# priv repo. Run once from the priv repo root:
#
#   scripts/secrets-migrate-from-git-crypt.sh
#
# What it does:
#   1. verifies a master age key exists (scripts/mk_age_master_key.sh)
#   2. moves every git-tracked plaintext secret from `<x>/secrets/` to
#      the parallel gitignored `<x>/secrets-plain/` tree and untracks it
#   3. hands over to scripts/secrets-encrypt.sh, which writes the
#      committed `<x>/secrets/*.age` ciphertext and the agenix manifest
#   4. drops git-crypt (.gitattributes filter lines, .git-crypt/ keyring)
#   5. extends .gitignore for the secrets-plain/ trees
#
# Idempotent: re-running only moves files that are still tracked, and
# secrets-encrypt.sh re-encrypts whatever changed. Committed *.age
# files and the manifest are preserved.
#
# NOTE: `nix` cannot read the moved plaintext at eval time from another
# machine. Kind-B consumers (builtins.readFile, imports of
# **/secrets-plain/**.nix) must be re-pointed by hand afterwards; the
# script lists them.

set -euo pipefail

repo="$(git rev-parse --show-toplevel)"
cd "$repo"

master_key="${AGE_MASTER_KEY:-$HOME/myconfig/.age-master.key}"

# ---- 1. master key ----------------------------------------------------

if [[ ! -f $master_key ]]; then
    echo "ERROR: no master age key at $master_key" >&2
    echo "generate one first: scripts/mk_age_master_key.sh" >&2
    exit 1
fi

# ---- 2. move tracked plaintext out of git -----------------------------

moved_nix=()
moved=0
while IFS= read -r -d '' f; do
    rel="${f#./}"

    # keep: agenix manifest, ciphertext, already-migrated trees
    [[ $rel == *.age ]] && continue
    [[ $rel == secrets/secrets.nix ]] && continue
    [[ $rel == *secrets-plain/* ]] && continue

    plain="${rel/\/secrets\///secrets-plain/}"
    if [[ $rel == secrets/* ]]; then
        plain="secrets-plain/${rel#secrets/}"
    fi

    if [[ -e $plain ]]; then
        if cmp -s "$rel" "$plain"; then
            git rm --cached --quiet -- "$rel"
            echo "untracked (plain already present): $rel"
        else
            echo "SKIP: $rel — $plain exists but differs, resolve manually" >&2
        fi
        continue
    fi

    mkdir -p "$(dirname "$plain")"
    mv "$rel" "$plain"
    git rm --cached --quiet -- "$rel"
    echo "moved: $rel -> $plain"
    moved=$((moved + 1))
    if [[ $rel == *.nix ]]; then
        moved_nix+=("$rel -> $plain")
    fi
done < <(git ls-files -z | grep -zE '(^|/)secrets/')

# ---- 3. ciphertext + manifest -----------------------------------------

"$(dirname "${BASH_SOURCE[0]}")/secrets-encrypt.sh"

# ---- 4. drop git-crypt --------------------------------------------------

if [[ -f .gitattributes ]] && grep -q git-crypt .gitattributes; then
    sed -i '/git-crypt/d' .gitattributes
    if [[ -s .gitattributes ]]; then
        git add .gitattributes
    else
        # the file was git-crypt only — drop it entirely
        git rm -qf .gitattributes
        echo "removed .gitattributes (was git-crypt only)"
    fi
    echo "removed git-crypt lines from .gitattributes"
fi
if [[ -e .git-crypt ]]; then
    git rm -r --cached --quiet .git-crypt 2>/dev/null || true
    rm -rf .git-crypt
    echo "removed .git-crypt/ keyring"
fi

# ---- 5. .gitignore ------------------------------------------------------

touch .gitignore
if ! grep -qxE '/?secrets-plain/?' .gitignore; then
    printf '/secrets-plain/\n' >>.gitignore
fi
if ! grep -qxE '\*\*/secrets-plain/?' .gitignore; then
    printf '**/secrets-plain/\n' >>.gitignore
fi
git add .gitignore

# ---- summary ------------------------------------------------------------

echo
echo "moved $moved file(s) out of git tracking."
if ((${#moved_nix[@]})); then
    echo
    echo "!! Nix files were moved — re-point their import/readFile sites:"
    for m in "${moved_nix[@]}"; do
        echo "   $m"
    done
fi
echo
echo "git-crypt history stays valid ciphertext in old commits; rotate any"
echo "secret whose git-crypt key file may have left this repository."
echo "Review with 'git status', then commit."
