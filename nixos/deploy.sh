#!/usr/bin/env bash
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

type "curl" &> /dev/null && {
  echo "* update hosts from someonewhocares.org ..."
  curl http://someonewhocares.org/hosts/hosts | \
    sed -e '/<localhost>/,/<\/localhost>/d' > static/extrahosts
  git update-index --assume-unchanged static/extrahost
}

set -e

# rsync file to target folder #############################################
echo "* rsync ..."
sudo rsync --filter="protect /hardware-configuration.nix" \
           --filter="protect /hostname" \
           --filter="protect /hostid" \
           --filter="exclude,s *.gitignore" \
           --filter="exclude,s *.gitmodules" \
           --filter="exclude,s *.git" \
           --filter="exclude .*.swp" \
           --delete --recursive --perms \
           "$SRC/" /etc/nixos/

# nixos-rebuild ###########################################################
echo "* nixos-rebuild ..."
sudo \
    NIX_CURL_FLAGS='--retry=1000' \
    nixos-rebuild --show-trace \
                  --upgrade \
                  --keep-failed \
                  --fallback ${1:-switch}

