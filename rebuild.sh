#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix ncurses git wget tmux glibcLocales
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
ROOT="$(pwd)"

. ./common.sh

###########################################################################
##  function  #############################################################
###########################################################################

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
                 "command echo \"... wrapped into tmux\"; NIX_PATH=\"$NIX_PATH\" $REBUILD_SH $@; read -t 1 -n 10000 discard; read -n 1 -s -r -p \"Press any key to continue\"" \; \
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
        logERR "not connected: ping heise.de failed"
        if ! wget -O - heise.de > /dev/null 2>&1; then
            logERR "not connected: wget heise.de failed"
            exit 1
        fi
    fi
}

isBranchMaster() {
    if [[ "$(cd "$ROOT"; git rev-parse --abbrev-ref HEAD)" == "master" ]]; then
        return 0
    else
        return 1
    fi
}

handleGit() {
    if isBranchMaster; then
        logH1 "update config" ""
        if git diff-index --quiet HEAD --; then
            git fetch
            local UPSTREAM='@{u}'
            local LOCAL=$(git rev-parse @)
            local REMOTE=$(git rev-parse "$UPSTREAM")
            local BASE=$(git merge-base @ "$UPSTREAM")
            if [ "x$LOCAL" == "x$REMOTE" ]; then
                echo "... up-to-date"
            elif [ "x$LOCAL" == "x$BASE" ]; then
                echo "* pull ..."
                git pull --rebase || true
                logH1 "run" "updatet version of script"
                exec $0
            elif [ "x$REMOTE" == "x$BASE" ]; then
                logINFO "need to push"
            else
                logERR "diverged"
            fi
        else
            logINFO "git directory is unclean"
            if [[ "$1" != "--fast" ]]; then
                read -t 10 -r -p "commit state as \"update before rebuild.sh\"? [y/N] " response || true
                if [[ "${response,,}" =~ ^(yes|y)$ ]]; then
                    git commit -am "update before rebuild.sh"
                    handleGit
                fi
            fi
        fi
    else
        logINFO "git branch is not master, do not handle git"
    fi
}

generateDiffFromTmpfiles() {
    oldFile=$1
    newFile=$2
    (set +e; sdiff -bBWs $oldFile $newFile |
        while read; do
            line="$REPLY"
            case $line in
                *'<'* ) echo "$(tput setaf 1)$line$(tput sgr0)" ;;
                *'>'* ) echo "$(tput setaf 2)$line$(tput sgr0)" ;;
                *'|'* ) echo "$(tput setaf 3)$line$(tput sgr0)" ;;
            esac
        done;
     rm $oldFile $newFile)
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
        generateDiffFromTmpfiles $oldFile $newFile
    else
        echo $newFile
    fi
}

diffGenerations() {
    local newFile=$(mktemp)
    { sudo nix-env --list-generations --profile /nix/var/nix/profiles/system || return; }> $newFile
    if [[ -f $1 ]]; then
        local oldFile="$1"
        logH2 "diff nixos generations"
        generateDiffFromTmpfiles $oldFile $newFile
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

handleGitPostExecution () {
    if isBranchMaster; then
        git diff --stat
        if ! git diff-index --quiet HEAD --; then
            read -r -p "commit state as \"update after rebuild.sh\"? [y/N] " response
            if [[ "${response,,}" =~ ^(yes|y)$ ]]; then
                git commit -am "update after rebuild.sh"
            fi
        fi
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
[[ "$1" == "--dry-run" ]] || {
    [[ "$1" != "--no-tmux" ]] && {
        wrapIntoTmux
    } || shift
    checkIfConnected
    # call sudo here, to ask for password early
    sudo echo "go ..."
    [[ "$1" != "--no-git" ]] && {
        handleGit
    } || shift
}

###########################################################################
# save current state and show them on exit ################################
currentSystemDeps=$(diffCurrentSystemDeps /run/current-system/)
currentUserDeps=$(diffCurrentSystemDeps ~/.nix-profile)
currentDiskUsage=$(diffDiskUsage)
currentNixosGenerations=$(diffGenerations)
startTime=$(date)

showStatDifferences() {
    logH1 "show" "stats"
    diffGenerations "$currentNixosGenerations"
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
# run #####################################################################
prepare() {
    if [[ ! -f "$myconfigDir/hostname" ]]; then
        echo "$myconfigDir/hostname should exist"
        exit 1
    fi
    if [[ ! -f "$myconfigDir/hostid" ]]; then
        echo "set hostid:"
        cksum /etc/machine-id |
            while read c rest; do printf "%x" $c; done |
            sudo tee "$myconfigDir/hostid"
    fi
    if [[ -f /etc/nixos/configuration.nix ]]; then
        echo "/etc/nixos/configuration.nix should not exist"
        exit 1
    fi

    nix_path_string="{ nix.nixPath = [\"nixpkgs=$nixpkgsDir\" \"nixos-config=$myconfigDir\"]; }"
    nix_path_file="$myconfigDir/imports/nixPath.nix"
    if [[ "$(cat $nix_path_file 2>/dev/null)" != *"$nix_path_string"* ]]; then
        echo $nix_path_string |
            tee $nix_path_file
    fi

    sudo mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/$my_user
}

realize() {
    if [[ "$1" == "--fast" ]]; then
        args="--fast"
    else
        args="--upgrade"
    fi

    local NIXOS_REBUILD_CMD=${NIXOS_REBUILD_CMD:-switch}
    logH1 "nixos-rebuild" "\$NIXOS_REBUILD_CMD=$NIXOS_REBUILD_CMD \$args=$args"
    logINFO "$NIX_PATH_ARGS"
    time sudo \
        NIX_CURL_FLAGS='--retry=1000' \
        nixos-rebuild \
        $NIX_PATH_ARGS \
        --show-trace --keep-failed \
        $args \
        --fallback \
        $NIXOS_REBUILD_CMD | sed -e 's/^/['"$args"'] /'
}

updateSubtree() {
    if ! git diff-index --quiet HEAD --; then
        logERR "uncommitted changes, do not update $channel"
    fi

    local remoteName=$1
    local remoteURL=$2
    local prefix=$3
    local branch=$4

    local remotes=$(git remote)
    if [[ "$remotes" != *"$remoteName"* ]]; then
        git remote add "$remoteName" "$remoteURL"
        git subtree split --rejoin --prefix="$prefix" HEAD
    fi

    git fetch "$remoteName" -- "$branch"
    logINFO "the channel $branch was last updated $(git log --format="%cr" remotes/$remoteName/$branch -1)"
    (set -x;
     git subtree pull --prefix $prefix "$remoteName" "$branch" --squash)
}

updateNixpkgs() {
    updateSubtree \
        NixOS-nixpkgs-channels https://github.com/NixOS/nixpkgs-channels \
        "nixpkgs" \
        "$nixStableChannel"
}

updateNixosHardware() {
    updateSubtree \
        NixOS-nixos-hardware https://github.com/NixOS/nixos-hardware \
        "nixos/hardware/nixos-hardware/" \
        "master"
}

update() {
    cd $myconfigDir
    if [[ "$MYCONFIG_ARGS" == *"--fast"* ]]; then
        logINFO "skip updating"
    else
        if [[ "$(cat $myconfigDir/hostname)" == "$my_main_host" ]]; then
            if git diff-index --quiet HEAD --; then
                logH1 "update" "nixpkgs"
                updateNixpkgs
                if git diff-index --quiet HEAD --; then
                    logH1 "update" "NixosHardware"
                    updateNixosHardware
                fi
            else
                logINFO "skip updating subtrees, not clean"
            fi

            logH3 "update" "home-manager"
            ./nixos/lib/home-manager/update.sh
            logH3 "update" "extrahosts"
            ./nixos/core/modules/nixos.networking/extrahosts/update.sh
            logH3 "update" "nixpkgs-unstable"
            ./nixos/lib/nixpkgs-unstable/update.sh
            logH3 "update" "emacs"
            ./nixos/base/modules/emacs/update.sh
            logH3 "update" "my-wallpapers"
            ./nixos/desktop/modules/common/my-wallpapers/update.sh
        fi
    fi
}

cleanup() {
    logH2 "cleanup" "nixos and nix-env"
    if [ "$((RANDOM%100))" -gt 90 ]; then
        echo "* nix-env --delete-generations 30d ..."
        nix-env $NIX_PATH_ARGS \
                --delete-generations 30d
        sudo nix-env $NIX_PATH_ARGS \
             --delete-generations 30d
        sudo nix-collect-garbage \
             --delete-older-than 30d
    else
        echo "* $(tput bold)do not$(tput sgr0) nix-env --delete-generations 30d ..."
    fi
}

handlePostExecutionHooks () {
    if [[ -d "$ROOT/misc/post_install_hooks" ]]; then
        find "$ROOT/misc/post_install_hooks" \
             -type f \
             -iname '*.sh' \
             -print \
             -exec {} \;
    fi
}

run() {
    if [[ "$MYCONFIG_ARGS" != *"--fast"* ]]; then
        if [[ "$1" == "--dry-run" ]]; then
            export NIXOS_REBUILD_CMD="dry-run"
        else
            prepare
            if [[ "$(cat $myconfigDir/hostname)" == "$my_main_host" ]]; then
                if isBranchMaster; then
                    realize --fast
                    update
                else
                    logINFO "git branch is not master, do not update"
                fi
            else
                logINFO "host is not main host, do not update"
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
}
run $@

# end run #################################################################
###########################################################################
[[ "$1" == "--dry-run" ]] || {
    showStatDifferences
    handleGitPostExecution
}
