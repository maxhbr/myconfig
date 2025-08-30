#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nixfmt-rfc-style git

set -euo pipefail

##############################################################################
## check arguments
##############################################################################

if [[ $# -gt 0 ]]; then
    arg="$1"
    if [[ -d "$arg" ]]; then
        cd "$arg"
        file="."
    elif [[ -f "$arg" ]]; then
        cd "$(dirname "$arg")"
        file="$(basename "$arg")"
    else
        echo "Invalid argument: $arg"
        exit 1
    fi
else
    cd "$(dirname "$0")"
    file="."
fi

##############################################################################
## check before running
##############################################################################

REASON_TO_NOT_DO_COMMIT=""

if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Not in a git repository"
    exit 1
else
    # Check if the working directory is clean
    if ! git diff-index --quiet HEAD --; then
        echo "Working directory is not clean"
        REASON_TO_NOT_DO_COMMIT="working directory is not clean"

        read -p "Do you want to continue? (y/n) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Aborting"
            exit 0
        fi
    fi
fi

##############################################################################
## running
##############################################################################

time find "$file" \
        -type f \
        -iname '*.nix' \
        -not -iname 'empty_nixos_config.nix' \
        -print \
        -exec nixfmt {} \;

##############################################################################
## commit after running
##############################################################################

if [ -z "$REASON_TO_NOT_DO_COMMIT" ]; then
    # Check if any files were modified
    if ! git diff-index --quiet HEAD --; then
        echo "Files were modified by nixfmt, committing changes..."
        git add -A
        git commit -m "nixfmtall"
        echo "Changes committed successfully"
    else
        echo "No files were modified by nixfmt"
    fi
else
    echo "Skipping commit because $REASON_TO_NOT_DO_COMMIT"
fi
