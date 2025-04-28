#!/usr/bin/env bash
set -euo pipefail

get_out_link_of_target() {
    local target="$1"; shift
    echo '../result.'"$target"
}
build() {
    local target="$1"; shift
    local out_link="$1"; shift
    echo "################################################################################"
    echo "building for $target to $out_link"
    echo "################################################################################"
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
get_ip_of_target_from_metadata() {
    local metadata_file="$1"; shift
    local target="$1"; shift
    local use_wg="${1:-false}";
    cat "$metadata_file" | 
        if [[ "$use_wg" == "true" ]]; then
            jq -r ".hosts.${target}.wireguard.wg0.ip4" || true
        else
            jq -r ".hosts.${target}.ip4" || true
        fi
}
get_ip_of_target() {
    local target="$1"; shift
    local use_wg="${1:-false}";
    ip="$(get_ip_of_target_from_metadata ./hosts/metadata.json "$target" "$use_wg")"
    if [[ "$ip" == "null" ]]; then
        ip="$(get_ip_of_target_from_metadata ../myconfig/hosts/metadata.json "$target" "$use_wg")"
    fi
    if [[ "$ip" == "null" ]]; then
        echo "ip for $target not found in metadata"
        exit 1
    fi
    echo "$ip"
}
copy_closure_to_target() {
    local targetIP="$1"; shift
    local out_link="$1"; shift
    echo "################################################################################"
    echo "copying closure to $targetIP"
    echo "################################################################################"
    set -x
    until nix-copy-closure --to "$targetIP" "$out_link"; do
        echo "... retry nix-copy-closure"
    done
    set +x
}
deploy() {
    local target="$1"; shift
    local out_link="$1"; shift
    echo "################################################################################"
    echo "deploying $out_link to $target"
    echo "################################################################################"
    local use_wg="${1:-false}";
    cmd="nixos-rebuild"
    if [[ "$target" != "$(hostname)" ]]; then
        targetIP="root@$(get_ip_of_target "$target" "$use_wg")"
        targetIP="${TARGET_IP:-"$targetIP"}"

        copy_closure_to_target "$targetIP" "$out_link"

        cmd="$cmd --target-host $targetIP"
    else
        cmd="sudo $cmd"
    fi

    export NIX_SSHOPTS='-o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'
    set -x
    until $cmd \
            `# --build-host localhost` \
            switch `#-p test` \
            --flake '.#'"$target"; do
        echo "... retry nixos-rebuild"
    done
    set +x
}
main() {
    local MODE=""
    if [[ $# -gt 0 && "$1" == "--fast" ]]; then
        MODE="$1"
        shift
    elif [[ $# -gt 0 && "$1" == "--test" ]]; then
        MODE="$1"
        shift
    elif [[ $# -gt 0 && "$1" == "--use-wg" ]]; then
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
  
    local token="$(pass github-bot-token2 -p || true)"
    if [[ ! -z "$token" ]]; then
     NIX_CONFIG="access-tokens = github.com=$token"
     export NIX_CONFIG
    fi

    if [[ "$MODE" == "" ]]; then
        cd "$(dirname "$0")"

        ( cd "../myconfig"; pwd; nix flake update --commit-lock-file )
        ( cd "../opossum.nix"; pwd; nix flake update --commit-lock-file )
        ( cd "../myphoto"; pwd; nix flake update --commit-lock-file )

        pwd
        nix flake update --commit-lock-file

        type gnupg-to-mutt.pl &> /dev/null && gnupg-to-mutt.pl
    else
        nix flake update myconfig
        nix flake update myphoto
        nix flake update mydwl
        nix flake update opossum
        nix flake update zephyr-flake
    fi

    local out_link="$(get_out_link_of_target "$target")"
    build "$target" "$out_link" || build "$target" "$out_link" --keep-failed --no-eval-cache
    du_of_out_link "$target"
    if [[ "$MODE" == "--use-wg" ]]; then
        deploy "$target" "$out_link" true
    elif [[ "$MODE" != "--test" ]]; then
        deploy "$target" "$out_link"
    fi
}

################################################################################

cd "$(dirname "$0")"
main "$@"
times
date
