#!/usr/bin/env bash

set -ex

cd "$(dirname $0)/.."

# git ls-remote --exit-code myphoto
git config remote.myphoto.url > /dev/null ||
    git remote add myphoto git@github.com:maxhbr/myphoto.git

if [[ ! -d myphoto ]]; then
    git subtree add --prefix myphoto myphoto master
else
    case $1 in
        pull) git subtree pull --prefix myphoto myphoto master ;;
        push) git subtree push --prefix myphoto myphoto master ;;
        *)
            cat<<EOF
usage:
   $0 pull
   $0 push
EOF
        ;;
    esac
fi
