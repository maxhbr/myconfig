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
DEPLOYMENT=myconfig-nixops

. ./common.sh
. ./rebuild.lib.wrapIntoTmux.sh
. ./rebuild.lib.diffState.sh
. ./rebuild.lib.logging.sh
. ./rebuild.lib.handleGit.sh

###########################################################################
##  variables  ############################################################
###########################################################################
export nixStableChannel=nixos-unstable

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
prepare_setup_nixops_deployment() {
    nixops info -d $DEPLOYMENT || nixops create -d $DEPLOYMENT ./nixops.nix
}

prepare_update_hostid_file() {
    if [[ ! -f "/etc/nixos/hostid" ]]; then
        echo "set hostid:"
        cksum /etc/machine-id |
            while read c rest; do printf "%x" $c; done |
            sudo tee "/etc/nixos/hostid"
    fi
}
prepare_update_hardware_configuration() {
    if [[ -f "/etc/nixos/hardware-configuration.nix" ]]; then
        if ! cmp "/etc/nixos/hardware-configuration.nix" "$nixosConfig/hardware-configuration.nix" >/dev/null 2>&1; then
            logH1 "update" "$nixosConfig/hardware-configuration.nix"
            cat "/etc/nixos/hardware-configuration.nix" | tee "$nixosConfig/hardware-configuration.nix"
        fi
    fi
}
prepare_update_nix_path_file() {
    nix_path_string="{ nix.nixPath = [ $(echo '"'"$NIX_PATH"'"' | sed 's/:/" "/g') ]; }"
    if [[ "$(cat $nix_path_file 2>/dev/null)" != *"$nix_path_string"* ]]; then
        logH1 "update" "$nix_path_file"
        echo "$nix_path_string" |
            tee "$nix_path_file"
    fi
}
prepare_create_nix_store_key() {
    # https://github.com/NixOS/nix/issues/2330#issuecomment-410505837
    if [[ ! -f ~/.config/nix/pk ]]; then
        logH1 "generate" "~/.config/nix/pk and ~/.config/nix/sk"
        mkdir -p ~/.config/nix/
        nix-store --generate-binary-cache-key machine.$(hostname) ~/.config/nix/sk ~/.config/nix/pk
        cat ~/.config/nix/pk
    fi
    # https://blog.joel.mx/posts/how-to-use-nix-copy-closure-step-by-step
    if [[ ! -f /etc/nix/signing-key.sec ]]; then
        logH1 "generate" "signing-key at /etc/nix/signing-key.sec and /etc/nix/signing-key.pub"
        (umask 277 && sudo openssl genrsa -out /etc/nix/signing-key.sec 2048)
        sudo openssl rsa -in /etc/nix/signing-key.sec -pubout | sudo tee /etc/nix/signing-key.pub
    fi
}
prepare_load_prefetches() {
    logH1 "prefetch" "$myconfigDir/prefetches/"
    find "$myconfigDir/prefetches/" \
          -not -path '*/\.*' \
         -type f \
         -exec  nix-prefetch-url "file://{}" \;
}
prepare() {
    if [[ -f /etc/nixos/configuration.nix ]]; then
        echo "/etc/nixos/configuration.nix should not exist"
        exit 1
    fi
    prepare_setup_nixops_deployment
    prepare_update_hostid_file
    prepare_update_hardware_configuration
    prepare_update_nix_path_file
    prepare_create_nix_store_key

    prepare_load_prefetches
}

realize() {
    if [[ "$1" == "--fast" ]]; then
        args="--fast"
        shift
    else
        args="--upgrade"
    fi

    local NIXOS_REBUILD_CMD=${NIXOS_REBUILD_CMD:-switch}
    logH1 "nixos-rebuild" "\$NIXOS_REBUILD_CMD=$NIXOS_REBUILD_CMD \$args=$args"
    logINFO "nixStableChannel=$nixStableChannel"
    logINFO "$NIX_PATH_ARGS"
    time sudo \
         -E \
         --preserve-env=NIX_PATH \
         NIX_CURL_FLAGS='-sS' \
         nixos-rebuild \
         $NIX_PATH_ARGS \
         --show-trace --keep-failed \
         $args \
         --fallback \
         $NIXOS_REBUILD_CMD | sed -e 's/^/['"$args"'] /'
    if [[ "$NIXOS_REBUILD_CMD" == "switch" ]]; then
        logH1 "nix path-info" "-hS /run/current-system"
        nix path-info -hS /run/current-system
    fi
}

updateNixpkgs() {
    logH1 "update nixpkgs" "nixStableChannel=$nixStableChannel"
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
        if [[ "$(hostname)" == "$my_main_host" ]]; then
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
            ./nixos/modules/nixos.networking/extrahosts/update.sh
            logH3 "update" "nixpkgs-unstable"
            ./nixos/lib/nixpkgs-unstable/update.sh
            logH3 "update" "emacs"
            ./nixos/modules/emacs/update.sh
            logH3 "update" "my-wallpapers"
            ./nixos/modules/desktop.common/my-wallpapers/update.sh
            logH3 "update" "chisui/zsh-nix-shell"
            ./nixos/modules/zsh/update.sh
        fi
    fi
}

cleanup() {
    logH2 "cleanup" "nixos and nix-env"
    pcent=$(echo $(df -h /  --output=pcent | tail -1 | cut -d'%' -f1))
    if [[ "$pcen" -gt 90 || "$((RANDOM%100))" -gt 90 ]]; then
        if [[ "$pcen" -gt 95 ]]; then
            ./nixos/gc.sh 15d
        else
            ./nixos/gc.sh
        fi
    else
        echo "* $(tput bold)do not$(tput sgr0) gc ..."
    fi
}

handlePostExecutionHooks () {
    if [[ -d "$myconfigDir/misc/post_install_hooks" ]]; then
        find "$myconfigDir/misc/post_install_hooks" \
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
            if [[ "$(hostname)" == "$my_main_host" ]]; then
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
    # nixops check -d $DEPLOYMENT
}
