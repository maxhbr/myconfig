#!/usr/bin/env bash
cd "$( dirname "${BASH_SOURCE[0]}" )/.."

set -e

if [[ ! -d "/mnt/etc/nixos/" ]]; then
    echo "folder /mnt/etc/nixos/ is missing"
    exit 1
fi

installBuiltSystem() {
    local target="$1"
    local outLink="/tmp/systemOutLink"

    set -x

    nix build \
        -L \
        --fallback \
        --log-format bar-with-logs \
        --out-link '../result.'"$target" \
        '.#nixosConfigurations.'"$target"'.config.system.build.toplevel'

    sudo nixos-install --no-root-passwd --system "$outLink"
}

installBuiltSystem "$@"
