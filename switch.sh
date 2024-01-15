#!/usr/bin/env bash
set -euo pipefail

get_out_link_of_target() {
    local target="$1"; shift
    echo '../result.'"$target"
}
build() {
    local target="$1"; shift
    local out_link="$(get_out_link_of_target "$target")"
    local system='.#nixosConfigurations.'"$target"'.config.system.build.toplevel'

    nix build \
        -L \
        --fallback \
        --log-format bar-with-logs \
        --out-link "$out_link" \
        --keep-going \
        $@ \
        "$system"
}
du_of_out_link() {
    local target="$1"; shift
    local out_link="$(get_out_link_of_target "$target")"
    nix path-info -rhsS "$out_link" |
        tee "$out_link"'.du' |
        tail -1
}
get_ip_of_target() {
    local target="$1"; shift
    ip="$(cat ./hosts/metadata.json | jq -r ".hosts.${target}.ip4")"
    if [[ "$ip" == "null" ]]; then
        ip="$(cat ../myconfig/hosts/metadata.json | jq -r ".hosts.${target}.ip4")"
    fi

    echo "$ip"
}
deploy() {
    local target="$1"; shift
    cmd="nixos-rebuild"
    if [[ "$target" != "$(hostname)" ]]; then
        targetIP="root@$(get_ip_of_target "$target")"
        targetIP="${TARGET_IP:-"$targetIP"}"
        cmd="$cmd --target-host $targetIP"

        local out_link="$(get_out_link_of_target "$target")"
        nix-copy-closure --to "$targetIP" "$out_link"
    else
        cmd="sudo $cmd"
    fi

    export NIX_SSHOPTS='-o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'
    until $cmd \
            `# --build-host localhost` \
            switch `#-p test` \
            --flake '.#'"$target"; do
        echo "... retry"
    done
}
main() {
    local MODE=""
    if [[ $# -gt 0 && "$1" == "--fast" ]]; then
        MODE="$1"
        shift
    elif [[ $# -gt 0 && "$1" == "--test" ]]; then
        MODE="$1"
        shift
    fi

    local target="${1:-$(hostname)}"

    ################################################################################
    # setup logging
    logsDir="../_logs"
    mkdir -p "$logsDir"
    logfile="$logsDir/$(date +%Y-%m-%d)-myconfig-${target}.log"
    echo -e "\n\n\n\n\n\n\n" >> "$logfile"
    exec &> >(tee -a "$logfile")
    ################################################################################

    set -x
    if [[ "$MODE" == "" ]]; then
        cd "$(dirname "$0")"

        ( cd "../myconfig"; pwd; nix flake update --commit-lock-file )
        ( cd "../opossum.nix"; pwd; nix flake update --commit-lock-file )
        ( cd "../myphoto"; pwd; nix flake update --commit-lock-file )

        pwd
        nix flake update --commit-lock-file

        type gnupg-to-mutt.pl &> /dev/null && gnupg-to-mutt.pl
    else
        nix flake lock --update-input myconfig
        nix flake lock --update-input mymyphoto
        nix flake lock --update-input mydwl
        nix flake lock --update-input opossum
    fi

    build "$target" || build "$target" --keep-failed --no-eval-cache
    du_of_out_link "$target"
    if [[ "$MODE" != "--test" ]]; then
        deploy "$target"
    fi
    set +x
}

################################################################################

cd "$(dirname "$0")"
main "$@"
times
date
