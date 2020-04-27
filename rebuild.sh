#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget tmux glibcLocales openssl nixops
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e
set -o pipefail
shopt -s lastpipe

if [ "$(id -u)" -ne "$(stat -c '%u' $0)" ]; then
    echo "you should run this script as the user, which owns $0"
    exit 1
fi

REBUILD_SH="$(readlink -f "${BASH_SOURCE[0]}")"
cd "$(dirname $REBUILD_SH)"
MYCONFIG_ARGS="$@"

###########################################################################
##  variables  ############################################################
###########################################################################
export DEPLOYMENT=myconfig-nixops
export nixStableChannel=nixos-unstable

. ./common.sh
. ./rebuild.lib.wrapIntoTmux.sh
. ./rebuild.lib.diffState.sh
. ./rebuild.lib.logging.sh
. ./rebuild.lib.handleGit.sh
. ./rebuild.action.prepare.sh
. ./rebuild.action.upgrade.sh
. ./rebuild.action.realize.sh
. ./rebuild.action.cleanup.sh

handlePostExecutionHooks () {
    if [[ -d "$myconfigDir/misc/post_install_hooks" ]]; then
        find "$myconfigDir/misc/post_install_hooks" \
             -type f \
             -iname '*.sh' \
             -print \
             -exec {} \;
    fi
}

###########################################################################
# prepare misc stuff ######################################################
[[ "$1" == "--dry-run" ]] || {
    [[ "$1" != "--no-tmux" ]] && {
        wrapIntoTmux "$REBUILD_SH"
    } || shift
    checkIfConnected
    # call sudo here, to ask for password early
    sudo echo "go ..."
    [[ "$1" != "--no-git" ]] && {
        handleGit
    } || shift
}
setupLoging
setupExitTrap "toplevel"

###########################################################################
# run #####################################################################
if [[ "$MYCONFIG_ARGS" != *"--fast"* ]]; then
    if [[ "$1" == "--dry-run" ]]; then
        export NIXOS_REBUILD_CMD="dry-run"
    else
        prepare
        if [[ "$(hostname)" == "$my_main_host" ]]; then
            if isBranchMaster; then
                realize --fast
                if [[ "$MYCONFIG_ARGS" == *"--fast"* ]]; then
                    logINFO "skip updating"
                else
                    upgrade
                fi
            else
                logINFO "git branch is not master, do not upgrade"
            fi
        else
            logINFO "host is not main host, do not upgrade"
        fi
    fi
    realize
    [[ "$1" == "--dry-run" ]] || {
        cleanup
        handlePostExecutionHooks
    }
else
    realize --fast
fi
# end run #################################################################
###########################################################################

[[ "$1" == "--dry-run" ]] || {
    showStatDifferences
    handleGitPostExecution
    # nixops check -d $DEPLOYMENT
}
