#!/usr/bin/env nix-shell
#! nix-shell -i bash -p git tig
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

DIR="$HOME"
GIT_DIR="$DIR/.myconfig.root.git"
cd "$DIR"

init_secrets_git() (
    local SECRETS_GIT_DIR="$1"
    mkdir -p "$SECRETS_GIT_DIR"
    cd "$SECRETS_GIT_DIR"
    git init --bare
    cd $(mktemp -d)
    git clone "$SECRETS_GIT_DIR" .
    touch README.md
    git add README.md
    git commit -am "initial commit"
    git push origin master
)

setup_on_demand() {
    if [[ ! -d "$GIT_DIR" ]]; then
        mkdir -p "$GIT_DIR"
        call_git init
        GIT_README="$HOME/.myconfig.root.README"
        touch "$GIT_README"
        call_git add "$GIT_README"
        call_git commit -m "initial commit"
        git submodule init
    fi

    MYCONFIG="$DIR/myconfig"
    if [[ ! -d "$MYCONFIG" ]]; then
        call_git submodule add "https://github.com/maxhbr/myconfig" myconfig
        cat<<EOF
add the following line to $HOME/.gitmodules:

[...]
  pushurl = git@github.com:maxhbr/myconfig.git
[...]
EOF
    fi

    SECRETS="$DIR/.myconfig.secrets"
    if [[ ! -d "$SECRETS" ]]; then
        if [[ ! -d "${SECRETS}.git" ]]; then
            init_secrets_git "${SECRETS}.git"
        fi
        call_git submodule add "${SECRETS}.git" .myconfig.secrets
    fi

#     GIT_MODULES="$DIR/.gitmodules"
#     if [[ ! -f "$GIT_MODULES" ]]; then
#         cat <<EOF > "$GIT_MODULES"
# [submodule "myconfig"]
# 	path = myconfig
#   url = https://github.com/maxhbr/myconfig
#   pushurl = git@github.com:maxhbr/myconfig.git
# [submodule "secrets"]
# 	path = .myconfig.secrets
#   url = $SECRETS
# EOF
#         call_git add "$GIT_MODULES"
#         call_git commit -m "add git modules file"
#     fi
}

call_git() {
    git --git-dir="$GIT_DIR" --work-tree="$DIR" "$@"
}

call_tig() {
    GIT_DIR="$GIT_DIR" GIT_WORK_TREE="$DIR" tig "$@"
}

set -x

setup_on_demand
if [[ "$1" == "tig" ]]; then
    shift
    call_tig "$@"
else
    call_git "$@"
fi
