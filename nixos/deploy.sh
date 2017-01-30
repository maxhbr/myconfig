#!/usr/bin/env bash
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

# hosts from someonewhocares.org ##########################################
type "curl" &> /dev/null && {
  echo "* update hosts from someonewhocares.org ..."

  [[ "$(find static/extrahosts -mtime +7)" != "" ]] && \
      curl http://someonewhocares.org/hosts/hosts | \
          sed -e '/<localhost>/,/<\/localhost>/d' > static/extrahosts
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
           --delete --recursive --perms --copy-links \
           "$SRC/" /etc/nixos/

# nixos-rebuild ###########################################################
echo "* nixos-rebuild ..."
exec sudo \
    NIX_CURL_FLAGS='--retry=1000' \
    nixos-rebuild --show-trace \
                  -I nixos-config=/etc/nixos/configuration.nix \
                  --upgrade \
                  --keep-failed \
                  --fallback ${1:-switch}
