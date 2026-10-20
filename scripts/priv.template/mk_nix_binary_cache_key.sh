#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix jq git

# Generate the nix binary cache signing key pair for `<hostname>`, a host
# that serves its store to other hosts via
# `myconfig.metadatalib.substituterHostFor`. Run from the top of the
# priv-repo checkout.
#
#   hosts/host.${hostname}/secrets/nix-binary-cache/
#   ├── secret.key   # deploy with myconfig.metadatalib.setupBinaryCacheSigning
#   └── public.key   # also written to the public repo's hosts/metadata.json
#
# After the script finishes:
#   1. In the priv overlay's module for ${hostname}, import
#        (myconfig.metadatalib.setupBinaryCacheSigning ./secrets/nix-binary-cache/secret.key)
#   2. Commit both repos and re-deploy ${hostname} and its clients.

set -euo pipefail
set -x

hostname="${1:?Usage: $0 <hostname>}"

cacheDir="hosts/host.${hostname}/secrets/nix-binary-cache"
myconfigRepo="${MYCONFIG_REPO:-/home/mhuber/myconfig/myconfig}"
metadataJson="${myconfigRepo}/hosts/metadata.json"

if [[ -d ${cacheDir} ]]; then
    echo "already exists!"
    exit 1
fi

mkdir -p "${cacheDir}"
nix-store --generate-binary-cache-key "${hostname}-1" \
    "${cacheDir}/secret.key" \
    "${cacheDir}/public.key"
chmod 0400 "${cacheDir}/secret.key"
git add "${cacheDir}"

publicKey="$(<"${cacheDir}/public.key")"
if [[ -f ${metadataJson} ]]; then
    tmp="$(mktemp)"
    jq --indent 2 --arg host "${hostname}" --arg key "${publicKey}" \
        '.hosts[$host].pubkeys["nix-binary-cache.pub"] = $key' \
        "${metadataJson}" >"${tmp}"
    cat "${tmp}" >"${metadataJson}"
    rm "${tmp}"
    echo "Added the public key to ${metadataJson}, commit it in the public repo."
else
    echo "WARN: ${metadataJson} not found, add the public key manually as" >&2
    echo "      hosts.${hostname}.pubkeys.\"nix-binary-cache.pub\" = \"${publicKey}\"" >&2
fi
