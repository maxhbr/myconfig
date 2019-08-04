#!/usr/bin/env nix-shell
#! nix-shell -i bash -p curl gitMinimal git-lfs
# Copyright 2016-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../common.sh"

gate() {
    nix-env --version &> /dev/null
}

upgrade() {
    logH3 "nix-rebuild" "declerative rebuild of nix userspace environment"
    nix-env $NIX_PATH_ARGS \
        --set -f "$(readlink -f "$(dirname ${BASH_SOURCE[0]})")" --argstr name "$(whoami)-user-env-$(date -I)"
}

gate || {
    echo "... skip"
    exit 0
}
if [ $# -eq 0 ]; then
    upgrade
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 0
    $@
fi
