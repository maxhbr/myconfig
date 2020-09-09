#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget glibcLocales openssl nixops nixfmt
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
DO_GIT=true
DO_UPGRADE=true
DO_ONLY_UPGRADE=false
DO_POST_STUFF=true
DRY_RUN=false
TARGET="$(hostname)"
TARGET_WAS_CHANGED=false
FORCE_RECREATE=false
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
    [--only-upgrade]   <--
    [--dry-run]        <-- don't touch anything
    [--target TARGET]  <-- deploy to other host
    [--force-recreate] <-- delete existing deployment
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
        --only-upgrade) shift
            DO_ONLY_UPGRADE=true
            ;;
        --dry-run) shift
            DO_UPGRADE=false
            DRY_RUN=true
            ;;
        --target) shift
            DO_UPGRADE=false
            TARGET=$1
            TARGET_WAS_CHANGED=true
            shift
            ;;
        --force-recreate) shift
            FORCE_RECREATE=true
            # implies --fast:
            DO_UPGRADE=false
            DO_POST_STUFF=false
            DO_GIT=false
            ;;
        --reboot) shift
            if $TARGET_WAS_CHANGED; then
              FORCE_REBOOT=true
              DO_UPGRADE=false
            fi
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
. ./rebuild.sh.d/rebuild.action.nixfmtall.sh
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
        if [[ ! $TARGET_WAS_CHANGED ]]; then
            nixfmtall
        fi
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
if $DO_ONLY_UPGRADE; then
    runWithTrap upgrade
else
    runWithTrap \
        realize $TARGET \
        "$($FORCE_RECREATE && echo "--force-recreate")" \
        "$($TARGET_WAS_CHANGED || echo "--is-local-host")" \
        "$($DRY_RUN && echo "--dry-run")" \
        "$($FORCE_REBOOT && echo "--force-reboot")"

    if ! $DRY_RUN; then
        if $DO_UPGRADE && isBranchMaster; then

            if upgrade; then
                logH1 "skip deploy" "nothing was updated, so no new realize run"
            else
                runWithTrap \
                    realize $TARGET \
                            "$($TARGET_WAS_CHANGED || echo "--is-local-host")" \
                            "$($DRY_RUN && echo "--dry-run")"

                if ! $TARGET_WAS_CHANGED; then
                    logINFO "reindex nix search"
                    nix search -u > /dev/null
                fi
            fi
        fi

        generateStats $TARGET
        if $DO_POST_STUFF; then
            runWithTrap cleanup
            runWithTrap handlePostExecutionHooks
        fi
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
