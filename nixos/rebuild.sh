#!/usr/bin/env bash

SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

# check, if connected #####################################################
if ! ping -c1 cloud.github.com > /dev/null 2>&1; then
  echo "**** not connected (ping) ****" >>$ERRFILE 2>&1
  # check again ###########################################################
  if ! wget -O - cloud.github.com > /dev/null 2>&1; then
    echo "**** not connected (wget) ****" >>$ERRFILE 2>&1
    exit 1
  fi
fi

# update git directory if clean ###########################################
echo "* update config ..."
if git diff-index --quiet HEAD --; then
    UPSTREAM=${1:-'@{u}'}
    LOCAL=$(git rev-parse @)
    REMOTE=$(git rev-parse "$UPSTREAM")
    BASE=$(git merge-base @ "$UPSTREAM")
    if [ $LOCAL = $REMOTE ]; then
        echo "Up-to-date"
    elif [ $LOCAL = $BASE ]; then
        echo "Need to pull"
        git pull
        exec $0
    elif [ $REMOTE = $BASE ]; then
        echo "Need to push"
    else
        echo "Diverged"
    fi
else
    echo "... your git directory is unclean, it will not be updated"
fi

# update git directory if clean ###########################################
#chmod 755 $SRC
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

# link dotfiles ###########################################################
echo "* dotfiles ..."
if [ "$EUID" -ne 0 ]; then
    [ -x ../dotfiles/deploy.sh ] && ../dotfiles/deploy.sh
fi

# set desktop background ##################################################
# TODO
