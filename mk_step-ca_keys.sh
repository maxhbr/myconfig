#!/usr/bin/env nix-shell
#! nix-shell -i bash -p step-cli python311Packages.xkcdpass git

# Generate the internal Certificate Authority key material for the
# step-ca instance on `<hostname>` (typically `vserver`). Run from the
# top of the priv-repo checkout. Mirrors the pattern established by
# scripts/priv.template/mk_tls_keypair.sh and mk_wireguard_keypair.sh.
#
# Output layout (inside the priv repo):
#
#   hosts/host.${hostname}/secrets/step-ca/
#   ├── password                     # intermediate-key passphrase
#   ├── root_ca.crt                  # public CA root cert
#   ├── root_ca_key                  # encrypted with root_ca_key_password
#   ├── root_ca_key_password         # keep offline / backup only
#   ├── intermediate_ca.crt          # public, signed by root
#   └── intermediate_ca_key          # encrypted with password
#
# The same root_ca.crt is also written into the public repo at
# modules/myconfig.deployedServices/internalCa/root.crt so every host's
# trust store picks up the real internal-CA root.
#
# After the script finishes:
#   1. Inspect the generated material under hosts/host.${hostname}/secrets/step-ca/
#   2. Add the per-secret `source = ./secrets/step-ca/<name>;` overrides
#      to the priv overlay's module for ${hostname} (see
#      mk_step-ca_keys.sh.README.md in the public repo).
#   3. Commit both repos.
#   4. Re-deploy ${hostname} (step-ca starts) and every Caddy host
#      (Caddy switches to ACME against the new CA).

set -euo pipefail
set -x

hostname="${1:-}"
if [[ -z ${hostname} ]]; then
    echo "usage: $0 <hostname>" >&2
    exit 1
fi

# Where the keys live inside the priv repo. Caller is expected to be in
# the priv repo root (consistent with the other mk_*.sh scripts).
caDir="hosts/host.${hostname}/secrets/step-ca"

# Where the public repo lives. Default mirrors the path used by
# scripts/priv.template/flake.nix:7. Override with MYCONFIG_REPO=... if
# your checkout is elsewhere.
myconfigRepo="${MYCONFIG_REPO:-/home/mhuber/myconfig/myconfig}"
publicRootCertPath="${myconfigRepo}/modules/myconfig.deployedServices/internalCa/root.crt"

# CA identity. Adjust if you want different names / SANs / curve.
caName="MyConfig Internal CA"
caRootSubject="${caName} Root"
caIntermediateSubject="${caName} Intermediate"

if [[ -d ${caDir} ]]; then
    echo "step-ca keys already exist at ${caDir}; refusing to overwrite." >&2
    echo "Delete the directory first if you really want to regenerate." >&2
    exit 1
fi

mkdir -p "${caDir}"

# ---- 1. Generate passphrases -----------------------------------------------
# `password` protects the intermediate key and is what step-ca reads at
# startup. `root_ca_key_password` protects the root key, which is only
# used during intermediate rotation; keep it backed up offline.
xkcdpass -n 8 >"${caDir}/password"
xkcdpass -n 8 >"${caDir}/root_ca_key_password"
chmod 0400 "${caDir}/password" "${caDir}/root_ca_key_password"

# ---- 2. Generate the root CA -----------------------------------------------
step certificate create \
    "${caRootSubject}" \
    "${caDir}/root_ca.crt" \
    "${caDir}/root_ca_key" \
    --profile root-ca \
    --kty EC --curve P-256 \
    --not-after 87600h \
    --password-file "${caDir}/root_ca_key_password"

# ---- 3. Generate the intermediate CA, signed by the root -------------------
step certificate create \
    "${caIntermediateSubject}" \
    "${caDir}/intermediate_ca.crt" \
    "${caDir}/intermediate_ca_key" \
    --profile intermediate-ca \
    --kty EC --curve P-256 \
    --not-after 35040h \
    --ca "${caDir}/root_ca.crt" \
    --ca-key "${caDir}/root_ca_key" \
    --ca-password-file "${caDir}/root_ca_key_password" \
    --password-file "${caDir}/password"

chmod 0400 \
    "${caDir}/root_ca_key" \
    "${caDir}/intermediate_ca_key"
chmod 0444 \
    "${caDir}/root_ca.crt" \
    "${caDir}/intermediate_ca.crt"

# ---- 4. Propagate the root cert into the public repo -----------------------
# The root certificate is the trust anchor every NixOS host installs
# via security.pki.certificateFiles. The placeholder committed in the
# public repo is short-lived; replace it with the real material here.
if [[ -d ${myconfigRepo} ]]; then
    cp "${caDir}/root_ca.crt" "${publicRootCertPath}"
    echo "Copied root_ca.crt -> ${publicRootCertPath}"
    echo "Remember to commit it in the public repo as well."
else
    echo "WARN: public repo not found at ${myconfigRepo}; skipping root.crt copy." >&2
    echo "      Copy ${caDir}/root_ca.crt manually into" >&2
    echo "      modules/myconfig.deployedServices/internalCa/root.crt" >&2
fi

# ---- 5. Stage in git -------------------------------------------------------
git add "${caDir}"
echo "Done. Inspect ${caDir}/ and add the priv-overlay source = ... wiring."
