#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# if [[ -f ../priv/switch.sh ]]; then
#   exit 1
# fi

################################################################################
# setup logging

if [[ $# -gt 0 && "$1" == "--fast" ]]; then
    shift
    nix flake update
else
    nix flake update --commit-lock-file
fi

target="${1:-$(hostname)}"

logsDir="../_logs"
mkdir -p "$logsDir"
logfile="$logsDir/$(date +%Y-%m-%d)-myconfig-${target}.log"
echo -e "\n\n\n\n\n\n\n" >> "$logfile"
exec &> >(tee -a "$logfile")

cmd="nixos-rebuild"
if [[ $# -gt 0 || -n "$TARGET_IP" ]]; then
    targetIP="root@$(cat ./hosts/metadata.json | jq -r ".hosts.${target}.ip4")"
    targetIP="${TARGET_IP:-"$targetIP"}"
    cmd="$cmd --target-host $targetIP"
else
    cmd="sudo $cmd"
fi

################################################################################
# run
set -x

time nix build \
    -L \
    --fallback \
    --keep-going \
    --keep-failed \
    --log-format bar-with-logs \
    --out-link '../result.'"$target" \
    '.#nixosConfigurations.'"$target"'.config.system.build.toplevel'


until $cmd \
        `#--build-host localhost` \
        switch `#-p test` \
        --flake '.#'"$target"; do
    echo "... retry"
done

set +x
times
date

# ./scripts/gc.sh
