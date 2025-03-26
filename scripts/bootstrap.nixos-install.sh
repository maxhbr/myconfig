#!/usr/bin/env bash
cd "$( dirname "${BASH_SOURCE[0]}" )/.."

set -euo pipefail

sendBuiltSystem() {
    local host="$1"
    local target="$2"
    local outLink='../result.'"$target"

    echo "send derivation for $target to IP=$host"

    set -x

    nix build \
        -L \
        --fallback \
        --log-format bar-with-logs \
        --out-link "$outLink" \
        '.#nixosConfigurations.'"$target"'.config.system.build.toplevel'

    nix-copy-closure --to "$USER@$host" "$outLink"

    cat <<EOF
run:
    sudo nixos-install --no-root-passwd --system "$outLink"
EOF
}

installBuiltSystem() {
    local target="$1"
    local outLink='../result.'"$target"

    set -x

    nix build \
        -L \
        --fallback \
        --log-format bar-with-logs \
        --out-link "$outLink" \
        '.#nixosConfigurations.'"$target"'.config.system.build.toplevel'

    sudo nixos-install --no-root-passwd --system "$outLink"
}

if [[ "$1" == "--send" ]]; then
  shift
  sendBuiltSystem "$@"
else
  if [[ ! -d "/mnt/etc/nixos/" ]]; then
      echo "folder /mnt/etc/nixos/ is missing"
      exit 1
  fi

  installBuiltSystem "$@"
fi
