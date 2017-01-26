#!/usr/bin/env bash
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

# hosts from someonewhocares.org ##########################################
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
           --filter="exclude,s *path*" \
           --filter="exclude,s *.gitmodules" \
           --filter="exclude,s *.git" \
           --filter="exclude .*.swp" \
           --delete --recursive --perms \
           "$SRC/" /etc/nixos/

# update channels #########################################################
echo "* update channels ..."
if git diff --cached --exit-code --quiet; then
    for submodule in path/nixos-16.09 path/nixos-unstable path/nixpkgs-unstable; do
        git submodule update --remote --merge $submodule
        git add $submodule
    done
    git commit -m "automatic update of submodules related to channels" 1>/dev/null || true
else
    echo "... something is staged: do not update channels automatically"
fi

# nixos-rebuild ###########################################################
echo "* nixos-rebuild ..."
exec sudo \
    NIX_CURL_FLAGS='--retry=1000' \
    nixos-rebuild --show-trace \
                  -I nixpkgs=http://nixos.org/channels/nixos-16.09/nixexprs.tar.xz \
                  -I nixos-config=/etc/nixos/configuration.nix \
                  -I "$SRC/path" \
                  --upgrade \
                  --keep-failed \
                  --fallback ${1:-switch}

