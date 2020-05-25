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

. ./common.sh

###########################################################################
##  variables  ############################################################
###########################################################################
DEPLOYMENT=myconfig-nixops
export NIXOPS_DEPLOYMENT=myconfig-nixops
export NIXOPS_STATE="$myconfigDir/nixops/secrets/myconfig-nixops.nixops"
nixStableChannel=nixos-unstable
DO_GIT=true
DO_UPGRADE=true
DO_POST_STUFF=true
USE_TMUX=true
DRY_RUN=false
TARGET="$(hostname)"
FORCE_REBOOT=false

ORIGINAL_ARGS="$@"
POSITIONAL=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --help) shift
            cat<<EOF
$0
    [--no-git]         <-- skip handnling of git
    [--fast]           <-- be fast ;)
    [--no-tmux]        <-- do not wrap into tmux
    [--dry-run]        <-- don't touch anything
    [--target TARGET]  <-- deploy to other host
    [--reboot]         <-- force reboot
EOF
            exit 0
            ;;

        --no-git) shift
            DO_GIT=false
            ;;
        --fast) shift
            DO_UPGRADE=false
            DO_POST_STUFF=false
            DO_GIT=false
            ;;
        --no-tmux) shift
            USE_TMUX=false
            ;;
        --dry-run) shift
            DO_UPGRADE=false
            USE_TMUX=false
            DRY_RUN=true
            ;;
        --target) shift
            DO_UPGRADE=false
            TARGET=$1
            shift
            ;;
        --reboot) shift
            FORCE_REBOOT=true
            ;;
        *) shift
            POSITIONAL+=("$1")
            ;;
    esac
done
set -- "${POSITIONAL[@]}"

###########################################################################
##  imports  ##############################################################
###########################################################################
setupExitTrap() {
    local msg=$1
    local cmd="code=\$?; if [[ \"\$code\" -ne 0 ]]; then logERR \"at $msg\"; logERR \"error code is: \$code\"; fi; exit \$code"
    trap "$cmd" EXIT ERR INT TERM
}

runWithTrap() {
    setupExitTrap "$1"
    $@
    setupExitTrap "---"
}
. ./rebuild.sh.d/rebuild.lib.wrapIntoTmux.sh
. ./rebuild.sh.d/rebuild.lib.logging.sh
. ./rebuild.sh.d/rebuild.lib.handleGit.sh
. ./rebuild.sh.d/rebuild.action.handleHomeGit.sh
. ./rebuild.sh.d/rebuild.action.prepare.sh
. ./rebuild.sh.d/rebuild.action.upgrade.sh
. ./rebuild.sh.d/rebuild.action.hooks.sh
. ./rebuild.sh.d/rebuild.action.realize.sh
. ./rebuild.sh.d/rebuild.action.cleanup.sh

###########################################################################
##  run  ##################################################################
###########################################################################

###########################################################################
# prepare misc stuff ######################################################
if ! $DRY_RUN; then
    if $USE_TMUX; then
        wrapIntoTmux "$REBUILD_SH" "$ORIGINAL_ARGS"
    fi
    checkIfConnected
    if $DO_GIT; then
        handleGit
        home_git_commit "start of rebuild.sh" || true
    fi
fi
setupLoging "$TARGET"
setupExitTrap "---"

###########################################################################
# core ####################################################################
runWithTrap prepare
if $DO_UPGRADE; then
    if [[ "$(hostname)" == "$my_main_host" ]]; then
        if isBranchMaster; then
            runWithTrap realize $TARGET $($DRY_RUN && echo "--dry-run")
            runWithTrap upgrade
        else
            logINFO "git branch is not master, do not upgrade"
        fi
    else
        logINFO "host is not main host, do not upgrade"
    fi
fi
runWithTrap realize $TARGET $($DRY_RUN && echo "--dry-run")  $($FORCE_REBOOT && echo "--force-reboot")
if ! $DRY_RUN; then
    generateStats $TARGET
    if $DO_POST_STUFF; then
        runWithTrap cleanup
        runWithTrap handlePostExecutionHooks
    fi
fi
# end core ################################################################
###########################################################################

if ! $DRY_RUN; then
    handleGitPostExecution
    home_git_commit "end of rebuild.sh" || true
fi
