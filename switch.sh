#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nvd
set -euo pipefail

cd "$(dirname "$0")"
verbose=""

guard_pid() {
    local target="$1"; shift
    local this_pid=$$
    local pidfile="/tmp/$(echo "$0" | sed 's/\//-/g').${target}.pid"
    if [[ -e "$pidfile" ]]; then
        old_pid="$(cat "$pidfile")"
        if [[ -e "/proc/$old_pid" ]]; then
            echo "process $old_pid is running for $target"
            exit 1
        fi
    fi
    echo "$this_pid" > "$pidfile"
}   

flake_update() (
    local path="$1"; shift
    echo "################################################################################"
    echo "update flake $path and commit lock file"
    echo "################################################################################"
    cd "$path";
    pwd
    set -x
    nix flake update ${verbose:+"--verbose"} --commit-lock-file
)
get_out_link_of_target() {
    local target="$1"; shift
    echo '../result.'"$target"
}
build() (
    local target="$1"; shift
    local out_link="$1"; shift
    echo "################################################################################"
    echo "building for $target to $out_link (at $(date))"
    echo "################################################################################"
    local system='.#nixosConfigurations.'"$target"'.config.system.build.toplevel'

    set -x
    nix build \
        ${verbose:+"--verbose"} \
        -L \
        --fallback \
        --log-format bar-with-logs \
        --out-link "$out_link" \
        --keep-going \
        $@ \
        "$system"
)
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

    if [[ "$use_wg" == "true" ]]; then
      local hosts_alias="$target.wg0"
      if grep -q " $hosts_alias"'$' /etc/hosts; then
        echo "$hosts_alias"
        return
      fi
    else
      if grep -q " $target\$" /etc/hosts; then
        echo "$target"
        return
      fi
    fi

    ip="$(get_ip_of_target_from_metadata ./hosts/metadata.json "$target" "$use_wg")"
    if [[ "$ip" == "null" ]]; then
        ip="$(get_ip_of_target_from_metadata ../myconfig/hosts/metadata.json "$target" "$use_wg")"
    fi
    if [[ "$ip" == "null" ]]; then
        echo "ip for $target not found in metadata"
        exit 1
    fi
    if [[ "$ip" == "" ]]; then
      echo "ip can not be empty"
      exit 1
    fi
    echo "$ip"
}
copy_closure_to_target() (
    local targetIP="$1"; shift
    local out_link="$1"; shift
    echo "################################################################################"
    echo "copying closure to $targetIP (at $(date))"
    echo "################################################################################"
    set -x
    until nix-copy-closure --to "$targetIP" "$out_link"; do
        echo "... retry nix-copy-closure"
    done
)
diff_build_results() (
    local old_result="$1"; shift
    local out_link="$1"; shift
    if command -v nvd &> /dev/null; then
        echo "################################################################################"
        echo "diffing $old_result and $out_link"
        echo "################################################################################"
        set -x
        nvd list -r "$out_link" > "$out_link"'.list'
        nvd diff "$old_result" "$out_link" | tee "$out_link"'.diff'
    fi
)
direct_deploy_locally() {
    local out_link="$1"; shift
    echo "################################################################################"
    echo "direct deploying $out_link to $(hostname) (at $(date))"
    echo "################################################################################"
    local store_path="$(nix-store -q "$out_link")"
    set -x
    sudo nix-env --profile /nix/var/nix/profiles/system --set "$store_path"
    sudo "$store_path/bin/switch-to-configuration" switch
}
deploy() (
    local target="$1"; shift
    local out_link="$1"; shift
    echo "################################################################################"
    echo "deploying $out_link to $target (at $(date))"
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
            ${verbose:+"--verbose"} \
            --flake '.#'"$target"; do
        echo "... retry nixos-rebuild"
    done
)
main() {
    local MODE=""
    if [[ $# -gt 0 && ("$1" == "--verbose" || "$1" == "-v") ]]; then
        verbose="--verbose"
        shift
    elif [[ $# -gt 0 && ("$1" == "--quiet" || "$1" == "-q") ]]; then
        verbose=""
        shift
    fi
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

    guard_pid "$target"

    ################################################################################
    # setup logging
    start_time="$(date +%s)"
    exec &> >(while read line; do current_time="$(date +%s)"; time_diff="$((current_time - start_time))"; printf "%5ds: %s\n" "$time_diff" "$line"; done)
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
        grep '\.url = "path:' flake.nix | sed s/.*path:// | sed 's/".*//g' |
            while read flake_path; do
                flake_update "$flake_path"
            done
        flake_update "."
        type gnupg-to-mutt.pl &> /dev/null && gnupg-to-mutt.pl
    else
        grep '\.url = "path:' flake.nix | sed s/\.url.*// | sed 's/ //g' |
            while read flake; do
                (set -x; nix flake update ${verbose:+"--verbose"} "$flake")
            done
    fi

    local out_link="$(get_out_link_of_target "$target")"
    local old_result="$(readlink -f "$out_link" || true)"
    build "$target" "$out_link" || build "$target" "$out_link" --keep-failed --no-eval-cache
    if [[ -e "$old_result" ]]; then
        diff_build_results "$old_result" "$out_link"
    fi
    du_of_out_link "$target"
    if [[ "$MODE" == "--use-wg" ]]; then
        deploy "$target" "$out_link" true
    elif [[ "$MODE" != "--test" ]]; then
        # if [[ "$target" == "$(hostname)" ]]; then
        #     direct_deploy_locally "$out_link"
        # else
            deploy "$target" "$out_link"
        # fi
    fi
}

################################################################################

main "$@"; times; date
