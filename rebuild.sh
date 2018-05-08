#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget tmux glibcLocales
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

###########################################################################
##  function  #############################################################
###########################################################################

have() { type "$1" &> /dev/null; }

logH1() {
    prefix=$1
    text=$2
    echo
    echo "$(tput bold)****************************************************************************"
    echo "***$(tput sgr0) $prefix $(tput bold)$text$(tput sgr0)"
}

logH2() {
    prefix=$1
    text=$2
    echo "$(tput bold)***$(tput sgr0) $prefix $(tput bold)$text$(tput sgr0)"
}

logH3() {
    prefix=$1
    text=$2
    echo "*** $prefix $(tput bold)$text$(tput sgr0)"
}

logINFO() {
    text=$1
    echo "$(tput setaf 3)$(tput bold)*** INFO: $(tput bold)$text$(tput sgr0)"
}

logERR() {
    text=$1
    echo "$(tput setaf 1)$(tput bold)*** ERR: $(tput bold)$text$(tput sgr0)"
}

runCmd() {
    folder=$1
    cmd=$2
    logH2 "Run" "$cmd of $folder"
    $folder/default.sh $cmd
}

wrapIntoTmux() {
    have "tmux" && {
        TMUX_NAME="rebuild_sh"
        if test -z $TMUX && [[ $TERM != "screen" ]]; then
            logH2 "wrap into tmux ..."
            tmux has-session -t $TMUX_NAME 2>/dev/null && {
                logERR "already running somewhere"
                exit 1
            }
            tmux -2 new-session -s $TMUX_NAME \
                 "command echo \"... wrapped into tmux\"; $REBUILD_SH $@; $SHELL" \; \
                 set-option status-left "rebuild.sh "\; \
                 set-option status-right "..."\; \
                 set set-titles-string "${TMUX_NAME}@tmux" \
                && exit 0
            logERR "tmux failed to start, running without tmux"
        fi
    } || {
        logINFO "tmux not installed"
    }
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
    BRANCH=$(git rev-parse --abbrev-ref HEAD)
    if [[ "$BRANCH" == "master" ]]; then
        logH1 "update config" ""
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

    newFile=$(mktemp)

    nix-store -qR $(readlink -f /run/current-system/) |
        # sed 's/^[^-]*-//g' |
        while read line ; do echo "$(tput bold)$(sed 's/^[^-]*-//g' <<< $line)$(tput sgr0) $line" ; done |
        sort -u > $newFile

    if [[ -f $1 ]]; then
        oldFile=$1

        logH1 "diff" "system dependencies"
        if ! diff -bdyZ --color=always -W 100 $oldFile $newFile | grep '\(<\|>\||\)'; then
            echo "... no diff"
        fi

        rm $oldFile $newFile
    else
        echo $newFile
    fi
}

###########################################################################
##  run  ##################################################################
###########################################################################

# prepare logging #########################################################
mkdir -p "${ROOT}/_logs/"
logfile="${ROOT}/_logs/$(date +%Y-%m-%d)-rebuild.sh.log"
echo -e "\n\n\n\n\n\n\n" >> $logfile
exec &> >(tee -a $logfile)

# misc ####################################################################
[[ "$1" != "--no-tmux" ]] &&\
    wrapIntoTmux
checkIfConnected
handleGit

# save current state of system dependencies ###############################
currentSystemDeps=$(diffCurrentSystemDeps)

# temporary use local configuration #######################################
logH1 "temporary" "link configurations to dev source"
runCmd ./nix deploy
runCmd ./nixos deploy

# run scripts #############################################################
logH1 "handle:" "prepare"
declare -a prepareFolders=("./nixos" "./nix" "./dotfiles" "./xmonad")
for folder in ${prepareFolders[@]}; do
    runCmd $folder prepare
done

logH1 "nix-build" "myconfig"
myconfig="$(nix-build default.nix --add-root myconfig --no-out-link -A myconfig)"

if [ -z "$myconfig" ]; then
    logERR "failed to build \$myconfig with nix"
    exit 1
fi

declare -a folders=("$myconfig/nixos" "$myconfig/nix" "./dotfiles") # "./xmonad"
declare -a commands=("deploy" "upgrade" "cleanup")
for cmd in ${commands[@]}; do
    logH1 "handle:" "$cmd"
    for folder in ${folders[@]}; do
        runCmd $folder $cmd
    done
done

# show differences in system dependencies #################################
diffCurrentSystemDeps $currentSystemDeps

# install nix package in place ############################################
logH1 "install" "$myconfig"
nix-env -i "$myconfig"
