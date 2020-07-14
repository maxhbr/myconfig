#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget glibcLocales openssl nixops
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

if [ ! -e "./secrets" ] ; then
    logERR "secrets symlink is DEAD, this is probably not the myconfig master"
    exit 1
fi

###########################################################################
##  variables  ############################################################
###########################################################################
export NIXOPS_STATE="$HOME/.myconfig-nixops.nixops"
nixStableChannel=nixos-unstable
nixUnstableChannel=nixpkgs-unstable
DO_GIT=true
DO_UPGRADE=true
DO_POST_STUFF=true
DRY_RUN=false
TARGET="$(hostname)"
FORCE_REBOOT=false
DO_SUSPEND=false

ORIGINAL_ARGS="$@"
POSITIONAL=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --help) shift
            cat<<EOF
$0
    [--no-git]         <-- skip handnling of git
    [--fast]           <-- be fast ;)
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
        --dry-run) shift
            DO_UPGRADE=false
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
        --suspend) shift
            DO_SUSPEND=true
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
    checkIfConnected
    if $DO_GIT; then
        handleGit
        home_git_commit "start of rebuild.sh" || true
    fi
fi

setupLoging() {
    local targetHost="$1"

    mkdir -p "$logsDir"
    local logfile="$logsDir/$(date +%Y-%m-%d)-myconfig-${targetHost}.log"
    echo -e "\n\n\n\n\n\n\n" >> $logfile
    exec &> >(tee -a $logfile)
}
setupLoging "$TARGET"

###########################################################################
# exit trap stuff #########################################################
setupExitTrap() {
    local msg=$1
    local cmd="code=\$?; if [[ \"\$code\" -ne 0 ]]; then logERR \"at $msg\"; logERR \"error code is: \$code\"; fi; exit \$code"
    trap "$cmd" EXIT ERR INT TERM
}
setupExitTrap "---"

runWithTrap() {
    setupExitTrap "$1"
    $@
    setupExitTrap "---"
}

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
    if $DO_SUSPEND; then
        runOnHost $TARGET "systemctl suspend"
    fi

    if $DO_GIT; then
        handleGitPostExecution
        home_git_commit "end of rebuild.sh" || true
    fi
fi
