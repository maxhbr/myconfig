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
    local oldCommit="$(git rev-parse HEAD)"
    if [[ ! -d $prefix ]]; then
        (set -x;
         git subtree add --prefix $prefix "$remoteName" "$branch" --squash)
    else
        (set -x;
         git subtree pull --prefix $prefix "$remoteName" "$branch" --squash)
    fi

    if [[ "$oldCommit" == "$(git rev-parse HEAD)" ]]; then
        return 0
    else
        return 1 # was updated
    fi
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
        "hardware/nixos-hardware/" \
        "master"
}

upgrade() {
    cd $myconfigDir
    if [[ "$(hostname)" == "$my_main_host" ]]; then
        logH1 "upgrade" "start ..."
        wasUpdated=0

        if git diff-index --quiet HEAD --; then
            logH1 "upgrade" "nixpkgs"
            upgradeNixpkgs || wasUpdated=1
            if git diff-index --quiet HEAD --; then
                logH1 "upgrade" "NixosHardware"
                upgradeNixosHardware || wasUpdated=1
            fi
        else
            logINFO "skip updating subtrees, not clean"
        fi

        logH3 "update" "home-manager"
        ./role.core/lib/home-manager/update.sh || wasUpdated=1
        logH3 "update" "nix-nixPath"
        ./role.core/lib/nix-nixPath/update.sh || wasUpdated=1
        logH3 "update" "extrahosts"
        ./role.core/nixos.networking/extrahosts/update.sh || wasUpdated=1
        logH3 "update" "emacs"
        ./role.desktop/emacs/spacemacs/update.sh || wasUpdated=1
        logH3 "update" "my-wallpapers"
        ./role.desktop/desktop.common/my-wallpapers/update.sh || wasUpdated=1
        logH3 "update" "chisui/zsh-nix-shell"
        ./role.core/zsh/update.sh || wasUpdated=1

        return $wasUpdated
    fi
}

export -f upgrade
