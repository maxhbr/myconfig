#!/usr/bin/env nix-shell
#! nix-shell -i bash -p openssl git

set -ex
hostname="$1"

tlsDir="hosts/host.${hostname}/secrets-plain/tls"

if [[ ! -d "$tlsDir" ]]; then
    mkdir -p "$tlsDir"
    openssl req -new -newkey rsa:4096 -days 365 -nodes -x509 \
        -keyout "$tlsDir/nginx.key" -out "$tlsDir/nginx.crt"
        # -subj "/C=country/ST=statet/L=city/O=organization/CN=${hostname}" \
    cat "$tlsDir/nginx.crt" "$tlsDir/nginx.key" \
           | tee "$tlsDir/nginx.pem"
    "$(dirname "$0")/scripts/secrets-encrypt.sh"
fi
