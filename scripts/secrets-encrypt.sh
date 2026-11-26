#!/usr/bin/env nix-shell
#! nix-shell -i bash -p age jq git

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# (Re-)encrypt every plaintext secret under **/secrets-plain/ into the
# committed **/secrets/ tree as *.age, and generate the agenix manifest
# secrets/secrets.nix.
#
# Recipients per file:
#   - the target host's ssh host public key, resolved from the merged
#     public (myconfig) + priv metadata like the flake's mkMetadata
#     (priv overrides, public contributes) — the same key
#     myconfig.secrets' mkEncryptedAgeFile encrypts to, so the file
#     decrypts with agenix's default age.identityPaths, plus
#   - the master age key (AGE_MASTER_KEY).
#
# Files not attributable to a host (secrets-plain/common/...) get the
# master key only. Set SECRETS_EXTRA_RECIPIENTS="age1... age1..." to
# encrypt to additional identities.
#
# Idempotent: overwrites the .age output only when the plaintext is
# newer than it, unless SECRETS_FORCE=1.

set -euo pipefail

repo="$(git rev-parse --show-toplevel)"
cd "$repo"

master_key="${AGE_MASTER_KEY:-$HOME/myconfig/.age-master.key}"
if [[ ! -f $master_key ]]; then
    echo "ERROR: no master age key at $master_key" >&2
    echo "generate one first: scripts/mk_age_master_key.sh" >&2
    exit 1
fi
master_pubkey="$(age-keygen -y "$master_key")"

# Host metadata is split like the flake's `mkMetadata` in the public
# repo (flake.lib.nix): the priv repo's hosts/metadata.json is passed
# as `metadataOverrides` on top of the public myconfig one
# (lib.recursiveUpdate public priv), so priv entries win but both
# contribute. Recreate the same merge here so host-key lookup sees
# the full picture.
public_metadata="${MYCONFIG_REPO:-$repo/../myconfig}/hosts/metadata.json"
if [[ ! -f $public_metadata ]]; then
    # fall back to the sibling layout used by the priv flake's
    # myconfig input (path:/home/mhuber/myconfig/myconfig)
    public_metadata="${MYCONFIG_REPO:-$HOME/myconfig/myconfig}/hosts/metadata.json"
fi
if [[ ! -f $public_metadata ]]; then
    echo "ERROR: public myconfig metadata not found" >&2
    echo "       expected at $public_metadata" >&2
    echo "       set MYCONFIG_REPO=<myconfig checkout>" >&2
    exit 1
fi
metadata="$(jq -n --slurpfile pub "$public_metadata" --slurpfile priv hosts/metadata.json '$pub[0] * $priv[0]')"

# Target host of a secrets-plain/ path:
#   hosts/host.<host>/secrets-plain/...  -> <host>
#   secrets-plain/<host>/...              -> <host> if known, else none
host_of() {
    local rel="$1"
    if [[ $rel == hosts/host.*/secrets-plain/* ]]; then
        sed -n 's|^hosts/host.\([^/]*\)/secrets-plain/.*|\1|p' <<<"$rel"
    elif [[ $rel == secrets-plain/*/* ]]; then
        local first
        first="$(cut -d/ -f2 <<<"$rel")"
        # only treat the first segment as a host if the merged metadata
        # knows it
        if [[ -n "$(jq -r --arg h "$first" '.hosts[$h].pubkeys // empty' <<<"$metadata")" ]]; then
            printf '%s' "$first"
        fi
    fi
}

pubkey_of() {
    jq -r --arg h "$1" \
        '.hosts[$h].pubkeys["/etc/ssh/ssh_host_rsa_key.pub"] // empty' \
        <<<"$metadata"
}

# Recipients of a plaintext file, one per line (never empty: the master
# key is always last).
recipients_for() {
    local host pk
    host="$(host_of "$1")"
    if [[ -n $host ]]; then
        pk="$(pubkey_of "$host")"
        if [[ -z $pk ]]; then
            echo "WARN: no ssh_host_rsa_key.pub for host '$host' in the merged" >&2
            echo "      metadata, encrypting '$1' to the master key only" >&2
        else
            printf '%s\n' "$pk"
        fi
    fi
    if [[ -n ${SECRETS_EXTRA_RECIPIENTS:-} ]]; then
        tr ' ' '\n' <<<"$SECRETS_EXTRA_RECIPIENTS"
    fi
    printf '%s\n' "$master_pubkey"
}

encrypt_one() {
    local rel="$1" out
    # nix files are not secrets: they stay tracked and unencrypted
    [[ $rel == *.nix ]] && return
    if [[ $rel == secrets-plain/* ]]; then
        out="secrets/${rel#secrets-plain/}"
    else
        out="${rel/secrets-plain/secrets}"
    fi
    out="$out.age"

    if [[ -e $out && $rel -ot $out && ${SECRETS_FORCE:-} != 1 ]]; then
        return
    fi

    local -a args=()
    while IFS= read -r r; do
        args+=(-r "$r")
    done < <(recipients_for "$rel")

    mkdir -p "$(dirname "$out")"
    age "${args[@]}" -o "$out" "$rel"
    git add -f -- "$out"
    echo "encrypted: $rel -> $out"
}

# ---- encrypt ------------------------------------------------------------

encrypted=0
while IFS= read -r -d '' f; do
    rel="${f#./}"
    [[ $rel == *.nix ]] && continue
    encrypt_one "$rel"
    encrypted=$((encrypted + 1))
done < <(find . -path ./.git -prune -o -type f -path '*/secrets-plain/*' -print0)
echo "processed $encrypted plaintext file(s)."

# ---- manifest ------------------------------------------------------------

# One top-level agenix manifest. Keys are paths relative to secrets/,
# so files in hosts/host.<h>/secrets/ get a ../ prefix.
manifest_key() {
    local rel="${1%.age}"
    if [[ $rel == secrets/* ]]; then
        printf '%s' "${rel#secrets/}"
    else
        printf '../%s' "$rel"
    fi
}

manifest="secrets/secrets.nix"
mkdir -p secrets

# collect entries first so host aliases can be declared in the let
# block above them

declare -A host_keys=()
entries=""
while IFS= read -r -d '' f; do
    rel="${f#./}"
    key="$(manifest_key "$rel")"
    host="$(host_of "${rel/secrets\//secrets-plain/}")"
    pk=""
    if [[ -n $host ]]; then
        pk="$(pubkey_of "$host")"
    fi
    if [[ -n $pk ]]; then
        host_keys["$host"]="$pk"
        printf -v entries '%s  "%s".publicKeys = [ hosts."%s" master ];\n' \
            "$entries" "$key" "$host"
    else
        printf -v entries '%s  "%s".publicKeys = [ master ];\n' "$entries" "$key"
    fi
done < <(git ls-files -z -- '*.age' | grep -zE '(^|/)secrets/')

{
    echo "# Generated by scripts/secrets-encrypt.sh — do not edit recipients"
    echo "# by hand; re-run the script instead. Each entry's publicKeys"
    echo "# list is the master key plus the target host's ssh host key"
    echo "# (resolved from the merged public+priv metadata at generation"
    echo "# time; host keys are public, so embedding them is fine)."
    echo "let"
    echo "  master = \"$master_pubkey\";"
    if ((${#host_keys[@]} > 0)); then
        echo "  hosts = {"
        while IFS= read -r h; do
            printf '    "%s" = "%s";\n' "$h" "${host_keys[$h]}"
        done < <(printf '%s\n' "${!host_keys[@]}" | sort)
        echo "  };"
    fi
    echo "in"
    echo "{"
    printf '%s' "$entries"
    echo "}"
} >"$manifest"
git add -f -- "$manifest"
echo "wrote $manifest"
