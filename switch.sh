#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

if [[ -f ../priv/switch.sh ]]; then
  exit 1
fi

################################################################################
# setup logging
logsDir="../_logs"
mkdir -p "$logsDir"
logfile="$logsDir/$(date +%Y-%m-%d)-myconfig-${target}.log"
echo -e "\n\n\n\n\n\n\n" >> "$logfile"
exec &> >(tee -a "$logfile")

# if [[ $# -gt 0 && "$1" == "--fast" ]]; then
#     shift
#     nix flake update
# else
#     ./update.sh
# fi

target="${1:-$(hostname)}"

cmd="nixos-rebuild"
if [ $# -gt 0 ]; then
    targetIP="root@$(cat ./hosts/metadata.json | jq -r ".hosts.${target}.ip4")"
    cmd="$cmd --target-host $targetIP"
else
    cmd="sudo $cmd"
fi

################################################################################
# run
set -x

nix build \
    -L \
    --fallback \
    --keep-going \
    --log-format bar-with-logs \
    --out-link '../result.'"$target" \
    '.#nixosConfigurations.'"$target"'.config.system.build.toplevel'


until $cmd \
        --build-host localhost \
        switch `#-p test` \
        --flake '.#'"$target"; do
    echo "... retry"
done

set +x

# ./scripts/gc.sh
