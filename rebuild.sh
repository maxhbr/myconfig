#!/usr/bin/env nix-shell
#! nix-shell -i zsh -p nix ncurses git wget tmux glibcLocales
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

if [ "$(id -u)" -ne "$(stat -c '%u' $0)" ]; then
    echo "you should run this script as the user, which owns $0"
    exit 1
fi

REBUILD_SH="$(readlink -f "${BASH_SOURCE[0]}")"
ROOT="$(dirname $REBUILD_SH)"
cd "$ROOT"

. common.sh

###########################################################################
##  function  #############################################################
###########################################################################

runCmd() {
    local folder=$1
    local cmd=$2
    logH2 "Run" "$cmd of $folder"
    $folder/default.sh $cmd
}

wrapIntoTmux() {
    have "tmux" && {
        local TMUX_NAME="rebuild_sh"
        if test -z $TMUX && [[ $TERM != "screen" ]]; then
            logH2 "wrap into tmux ..."
            tmux has-session -t $TMUX_NAME 2>/dev/null && {
                logERR "already running somewhere"
                exit 1
            }
            tmux -2 new-session -s $TMUX_NAME \
                 "command echo \"... wrapped into tmux\"; $REBUILD_SH $@; read -t 1 -n 10000 discard; read -n 1 -s -r -p \"Press any key to continue\"" \; \
                 set-option status-left "rebuild.sh"\; \
                 set-option status-right "started at $(date) "\; \
                 set set-titles-string "${TMUX_NAME}@tmux" \
                && exit 0
            logERR "tmux failed to start, running without tmux"
        fi
    } || logINFO "tmux not installed"
}

checkIfConnected() {
    if ! ping -c1 heise.de > /dev/null 2>&1; then
        logERR "not connected: ping"
        if ! wget -O - heise.de > /dev/null 2>&1; then
            logERR "not connected: wget"
            exit 1
        fi
    fi
}

handleGit() {
    local BRANCH=$(git rev-parse --abbrev-ref HEAD)
    if [[ "$BRANCH" == "master" ]]; then
        logH1 "update config" ""
        if git diff-index --quiet HEAD --; then
            git fetch
            local UPSTREAM=${1:-'@{u}'}
            local LOCAL=$(git rev-parse @)
            local REMOTE=$(git rev-parse "$UPSTREAM")
            local BASE=$(git merge-base @ "$UPSTREAM")
            if [ $LOCAL = $REMOTE ]; then
                echo "... up-to-date"
            elif [ $LOCAL = $BASE ]; then
                echo "* pull ..."
                git pull --rebase || continue
                logH1 "run" "updatet version of script"
                exec $0
            elif [ $REMOTE = $BASE ]; then
                logINFO "need to push"
            else
                logERR "diverged"
            fi
        else
            logINFO "git directory is unclean, it will not be updated"
        fi
    else
        logINFO "git branch is not master, do not handle git"
    fi
}

diffCurrentSystemDeps() {
    have nix-store || return 0
    [[ -e $1 ]] || return 0

    local profileRoot=$1
    local newFile=$(mktemp)

    nix-store -qR $profileRoot |
        sed 's/^[^-]*-//g' |
        # while read line ; do echo "$(sed 's/^[^-]*-//g' <<< $line) $line" ; done |
        sort -u > $newFile

    if [[ -f $2 ]]; then
        local oldFile=$2

        logH2 "diff dependencies of $profileRoot"

        sdiff -bBWs $oldFile $newFile |
            while read; do
                line="$REPLY"
                case $line in
                    *'<'* ) echo "$(tput setaf 1)$line$(tput sgr0)" ;;
                    *'>'* ) echo "$(tput setaf 2)$line$(tput sgr0)" ;;
                    *'|'* ) echo "$(tput setaf 3)$line$(tput sgr0)" ;;
                esac
            done

        rm $oldFile $newFile
    else
        echo $newFile
    fi
}

diffDiskUsage() {
    [[ -e /dev/dm-2 ]] || return 0

    local newFile=$(mktemp)
    df -h --output="pcent,used" /dev/dm-2 > $newFile

    if [[ -f $1 ]]; then
        local oldFile=$1

        logH2 "diff disk usage"
        sdiff -bBW $oldFile $newFile

        rm $oldFile $newFile
    else
        echo $newFile
    fi
}

###########################################################################
##  run  ##################################################################
###########################################################################

###########################################################################
# prepare logging #########################################################
mkdir -p "${ROOT}/_logs/"
logfile="${ROOT}/_logs/$(date +%Y-%m-%d)-rebuild.sh.log"
echo -e "\n\n\n\n\n\n\n" >> $logfile
exec &> >(tee -a $logfile)

###########################################################################
# prepare misc stuff ######################################################
[[ "$1" != "--no-tmux" ]] && {
    wrapIntoTmux
} || shift
checkIfConnected
handleGit

###########################################################################
# save current state and show them on exit ################################
currentSystemDeps=$(diffCurrentSystemDeps /run/current-system/)
currentUserDeps=$(diffCurrentSystemDeps ~/.nix-profile)
currentDiskUsage=$(diffDiskUsage)
startTime=$(date)

showStatDifferences() {
    logH1 "show" "stats"
    diffCurrentSystemDeps /run/current-system/ $currentSystemDeps
    diffCurrentSystemDeps ~/.nix-profile $currentUserDeps
    diffDiskUsage $currentDiskUsage
    echo "... start: $startTime"
    echo "...   end: $(date)"
}

setupExitTrap() {
    local msg=$1
    local cmd="code=\$?; if [[ \"\$code\" -ne 0 ]]; then logERR \"at $msg\"; logERR \"error code is: \$code\"; fi; showStatDifferences; exit \$code"
    trap "$cmd" EXIT ERR INT TERM
}

###########################################################################
# run scripts #############################################################
declare -a folders=("./nix" "./nixos" "./dotfiles" "./xmonad")
declare -a commands=("prepare" "deploy" "upgrade" "cleanup")
for cmd in ${commands[@]}; do
    logH1 "handle:" "$cmd"
    for folder in ${folders[@]}; do
        setupExitTrap "$cmd for $folder"
        MYCONFIG_ARGS=$@ runCmd $folder $cmd
    done
done
trap "" EXIT ERR INT TERM
showStatDifferences
