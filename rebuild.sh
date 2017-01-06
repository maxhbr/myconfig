#!/usr/bin/env bash
#
#  written by maximilian-huber.de

set -e

if [ "$EUID" -eq 0 ]; then
    echo "you should run this script as user"
    exit 1
fi

SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SRC

# check, if connected #####################################################
if ! ping -c1 cloud.github.com > /dev/null 2>&1; then
  echo "**** not connected (ping) ****"
  # check again ###########################################################
  if ! wget -O - cloud.github.com > /dev/null 2>&1; then
    echo "**** not connected (wget) ****"
    exit 1
  fi
fi

# update git directory if clean ###########################################
echo "* update config ..."
if git diff-index --quiet HEAD --; then
    git fetch
    UPSTREAM=${1:-'@{u}'}
    LOCAL=$(git rev-parse @)
    REMOTE=$(git rev-parse "$UPSTREAM")
    BASE=$(git merge-base @ "$UPSTREAM")
    if [ $LOCAL = $REMOTE ]; then
        echo "... up-to-date"
    elif [ $LOCAL = $BASE ]; then
        echo "* pull ..."
        git pull --rebase
        # run updatet version of script ###################################
        exec $0
    elif [ $REMOTE = $BASE ]; then
        echo "... need to push"
    else
        echo "... diverged"
    fi
else
    echo "... your git directory is unclean, it will not be updated"
fi

# run hooks ###############################################################
shopt -s nullglob
for f in $SRC/_hooks/*; do
    [ -x $f ] && $f
done
