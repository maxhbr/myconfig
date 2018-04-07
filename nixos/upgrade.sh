#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# hosts from someonewhocares.org ##########################################
type "curl" &> /dev/null && {
    [[ ! -f static/extrahosts || "$(find static/extrahosts -mtime +1)" != "" ]] && {
        echo "* $(tput bold)update hosts blacklist$(tput sgr0) ..."
        # curl http://someonewhocares.org/hosts/hosts | \
            #     sed -e '/<localhost>/,/<\/localhost>/d' > static/extrahosts
        # use hosts file from https://github.com/StevenBlack/hosts (MIT)
        curl https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts |
            grep ^0 > static/extrahosts
    }
}

set -e

# nixos-rebuild ###########################################################
echo "* $(tput bold)nixos-rebuild$(tput sgr0) ..."
NIX_PATH=
exec sudo \
     NIX_CURL_FLAGS='--retry=1000' \
     nixos-rebuild --show-trace \
     -I nixpkgs=http://nixos.org/channels/nixos-18.03/nixexprs.tar.xz \
     -I nixpkgs-overlays=/etc/nix/overlays \
     -I nixos-config=/etc/nixos/configuration.nix \
     --upgrade \
     --keep-failed \
     --fallback ${1:-switch}
