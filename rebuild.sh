#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
#  written by maximilian-huber.de
set -e

if [ "$EUID" -eq 0 ]; then
    echo "you should run this script as user"
    exit 1
fi

log() {
    prefix=$1
    text=$2
    echo
    echo "$(tput bold)****************************************************************************"
    echo "***$(tput sgr0) $prefix $(tput bold)$text$(tput sgr0)"
}
runCmd() {
    script=$1
    log "Run" "$script"
    $script
}

REBUILD_SH="$(readlink -f "${BASH_SOURCE[0]}")"
ROOT="$(dirname $REBUILD_SH)"
cd "$ROOT"

# wrap into tmux ##########################################################
TMUX_NAME="rebuild_sh"
if test -z $TMUX && [[ $TERM != "screen" ]]; then
    log "wrap into tmux ..."
    set -x
    tmux has-session -t $TMUX_NAME 2>/dev/null && {
        echo "already running somewhere"
        exit 1
    }
    tmux -2 new-session -s $TMUX_NAME \
         "command echo \"... wrapped into tmux\"; $REBUILD_SH $@; $SHELL" \; \
         set-option status-left "rebuild.sh "\; \
         set-option status-right "..."\; \
         set set-titles-string "${TMUX_NAME}@tmux" \
        && exit 0
    echo "tmux failed to start, running without tmux"
fi

# prepare logging #########################################################
mkdir -p "${ROOT}/_logs/"
logfile="${ROOT}/_logs/$(date +%Y-%m-%d)-rebuild.sh.log"
echo -e "\n\n\n\n\n\n\n" >> $logfile
exec &> >(tee -a $logfile)

# check, if connected #####################################################
if ! ping -c1 heise.de > /dev/null 2>&1; then
    log "not connected" "ping"
    # check again ###########################################################
    if ! wget -O - heise.de > /dev/null 2>&1; then
        log "not connected" "wget"
        exit 1
    fi
fi

# update git directory if clean ###########################################
log "update config" ""
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
        git pull --rebase || continue
        # run updatet version of script ###################################
        exec $0
    elif [ $REMOTE = $BASE ]; then
        echo "... need to push"
    else
        echo "... diverged"
    fi
else
    echo "... git directory is unclean, it will not be updated"
fi

# run scripts #############################################################
runCmd "./nixos/prepare.sh"
log "nix-build" "myconfig"
myconfig="$(nix-build default.nix  --add-root myconfig -A myconfig)"
log "install" "$myconfig"
nix-env -i ./result
declare -a scripts=(
    "$myconfig/nix/_deploy.sh"
    "$myconfig/nixos/_deploy.sh"
    "$myconfig/nixos/_upgrade.sh"
    "$myconfig/nixos/_cleanup.sh"
    "$myconfig/nix/_cleanup.sh"
    "$myconfig/nix/_upgrade.sh"
    "./dotfiles/_deploy.sh"
    "./xmonad/_deploy.sh"
    "$myconfig/bin/create_and_update_repos.pl"
    "$myconfig/bin/xrdb_merge.sh"
   )
for script in ${scripts[@]}; do
    runCmd $script
done
