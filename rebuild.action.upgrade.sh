. ./common.sh

upgradeSubtree() {
    if ! git diff-index --quiet HEAD --; then
        logERR "uncommitted changes, do not upgrade $channel"
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
    logINFO "the channel $branch was last upgraded $(git log --format="%cr" remotes/$remoteName/$branch -1)"
    (set -x;
     git subtree pull --prefix $prefix "$remoteName" "$branch" --squash)
}

upgradeNixpkgs() {
    logH1 "upgrade nixpkgs" "nixStableChannel=$nixStableChannel"
    upgradeSubtree \
        NixOS-nixpkgs-channels https://github.com/NixOS/nixpkgs-channels \
        "nixpkgs" \
        "$nixStableChannel"
}

upgradeNixosHardware() {
    upgradeSubtree \
        NixOS-nixos-hardware https://github.com/NixOS/nixos-hardware \
        "nixos/hardware/nixos-hardware/" \
        "master"
}

upgrade() {
    cd $myconfigDir
    if [[ "$(hostname)" == "$my_main_host" ]]; then
        if git diff-index --quiet HEAD --; then
            logH1 "upgrade" "nixpkgs"
            upgradeNixpkgs
            if git diff-index --quiet HEAD --; then
                logH1 "upgrade" "NixosHardware"
                upgradeNixosHardware
            fi
        else
            logINFO "skip updating subtrees, not clean"
        fi

        logH3 "upgrade" "home-manager"
        ./nixos/lib/home-manager/upgrade.sh
        logH3 "upgrade" "extrahosts"
        ./nixos/modules/nixos.networking/extrahosts/upgrade.sh
        logH3 "upgrade" "nixpkgs-unstable"
        ./nixos/lib/nixpkgs-unstable/upgrade.sh
        logH3 "upgrade" "emacs"
        ./nixos/modules/emacs/upgrade.sh
        logH3 "upgrade" "my-wallpapers"
        ./nixos/modules/desktop.common/my-wallpapers/upgrade.sh
        logH3 "upgrade" "chisui/zsh-nix-shell"
        ./nixos/modules/zsh/upgrade.sh
    fi
}

export -f upgrade
