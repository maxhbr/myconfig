#!/usr/bin/env bash
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

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

# rsync file to target folder #############################################
echo "* $(tput bold)rsync$(tput sgr0) ..."
sudo rsync --filter="protect /hardware-configuration.nix" \
           --filter="protect /hostname" \
           --filter="protect /hostid" \
           --filter="exclude,s *.gitignore" \
           --filter="exclude,s *.gitmodules" \
           --filter="exclude,s *.git" \
           --filter="exclude .*.swp" \
           --filter="exclude .#*" \
           --filter="exclude result" \
           --delete --recursive --perms --copy-links \
           "$SRC/" /etc/nixos/

# nixos-rebuild ###########################################################
echo "* $(tput bold)nixos-rebuild$(tput sgr0) ..."
exec sudo \
    NIX_CURL_FLAGS='--retry=1000' \
    NIX_PATH='nixpkgs=http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz:unstable=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz:nixpkgs-overlays=/etc/nix/overlays:nixos-config=/etc/nixos/configuration.nix' \
    nixos-rebuild --show-trace \
                  -I nixpkgs=http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz \
                  -I unstable=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz \
                  -I nixpkgs-overlays=/etc/nix/overlays \
                  -I nixos-config=/etc/nixos/configuration.nix \
                  --upgrade \
                  --keep-failed \
                  --fallback ${1:-switch}
