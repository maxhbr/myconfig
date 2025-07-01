#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
verbose=""
ulimit -c unlimited

have() {
    local cmd="$1"; shift
    if command -v "$cmd" &> /dev/null; then
        return 0
    fi
    return 1
}

log_step() {
    {
        echo "$(tput setaf 2)################################################################################$(tput sgr0)"
        echo "$1 (at $(date))"
        echo "$(tput setaf 2)################################################################################$(tput sgr0)"
    } >&2
}
log_warning() {
    echo "$(tput setaf 3)warning: $1$(tput sgr0)" >&2
}
log_error() {
    echo "$(tput setaf 1)error: $1$(tput sgr0)" >&2
}

guard_pid() {
    local target="$1"; shift
    local this_pid=$$
    local pidfile="/tmp/$(echo "$0" | sed 's/\//-/g').${target}.pid"
    if [[ -e "$pidfile" ]]; then
        old_pid="$(cat "$pidfile")"
        if [[ -e "/proc/$old_pid" ]]; then
            log_error "process $old_pid is running for $target"
            exit 1
        fi
    fi
    echo "$this_pid" > "$pidfile"
}   

flake_update() {
    local update_mode="$1"; shift
    log_step "updating flake in $update_mode mode"

    if [[ "$update_mode" == "full" ]]; then
        flake_update_recursively "."
        type gnupg-to-mutt.pl &> /dev/null && gnupg-to-mutt.pl
    elif [[ "$update_mode" == "fast" ]]; then
        flake_update_one "."
        # grep '\.url = "path:' flake.nix | sed s/\.url.*// | sed 's/ //g' |
        #     while read flake; do
        #         (set -x; nix flake update ${verbose:+"--verbose"} "$flake")
        #     done
    else
        log_error "unknown update mode: $update_mode"
        exit 1
    fi

}

flake_update_recursively() (
    local path="$1"; shift
    local flake="$path/flake.nix"
    echo ">> recursively walk flake $path"
    if [[ ! -e "$flake" ]]; then
        log_error "flake.nix not found in $path"
        return
    fi
    if grep -q '\.url = "path:' "$flake"; then
        grep '\.url = "path:' "$flake" | sed s/.*path:// | sed 's/".*//g' |
            while read flake_path; do
                flake_update_recursively "$flake_path"
            done
    fi
    flake_update_one "$path"
)

flake_update_one() (
    local path="$1"; shift
    local num_of_tries="${1:-1}";
    echo ">>> update flake $path and commit lock file"
    if [[ "$num_of_tries" -gt 1 ]]; then
        log_warning "retry $num_of_tries times"
    fi
    cd "$path";
    set -x
    nix flake update ${verbose:+"--verbose"} --commit-lock-file || {
        set +x
        if [[ "$num_of_tries" -lt 3 ]]; then
            flake_update_one "$path" $((num_of_tries + 1))
        else
            log_error "failed to update flake $path"
            exit 1
        fi
    } 
)
get_out_link_of_target() {
    local target="$1"; shift
    echo '../result.'"$target"
}
build() (
    local target="$1"; shift
    local out_link="$1"; shift
    log_step "building for $target to $out_link"
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
    cat "$out_link"'.du' |
        gawk '
        BEGIN {FS="\t"; OFS=","} {
            if ($3 ~ / *[0-9\.]+ KiB/)
                { a=gensub(/ *([0-9\.]+)( KiB)/,"\\1","g",$3); \
                printf "%s KiB,%s,%s,%s\n", a,$3,$2,$1} \
            else if ($3 ~ / *[0-9\.]+ MiB/)
                { a=gensub(/ *([0-9\.]+)( MiB)/,"\\1","g",$3); \
                printf "%s KiB,%s,%s,%s\n", a*1024,$3,$2,$1} \
            else if ($3 ~ / *[0-9\.]+ GiB/)
                { a=gensub(/ *([0-9\.]+)( GiB)/,"\\1","g",$3); \
                printf "%s KiB,%s,%s,%s\n", a*1024*1024,$3,$2,$1} \
            else {print "?",$3,$2,$1}
            }
        ' |
        sort -n |
        awk 'BEGIN {FS=","; OFS=","} {print $2,$3,$4}' > "$out_link"'.du.csv'
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
        log_error "ip for $target not found in metadata"
        exit 1
    fi
    if [[ "$ip" == "" ]]; then
      log_error "ip can not be empty"
      exit 1
    fi
    echo "$ip"
}
copy_closure_to_target() (
    local targetIP="$1"; shift
    local out_link="$1"; shift
    log_step "copying closure to $targetIP"
    set -x
    until nix-copy-closure --to "$targetIP" "$out_link"; do
        log_warning "retry nix-copy-closure"
    done
)
diff_build_results() (
    local old_result="$1"; shift
    local out_link="$1"; shift
    if command -v nvd &> /dev/null; then
        log_step "diffing $old_result and $out_link"
        set -x
        nvd list -r "$out_link" > "$out_link"'.list'
        nvd diff "$old_result" "$out_link" | tee "$out_link"'.diff'
    fi
)
direct_deploy_locally() {
    local out_link="$1"; shift
    log_step "direct deploying $out_link to $(hostname)"
    local store_path="$(nix-store -q "$out_link")"
    set -x
    sudo nix-env --profile /nix/var/nix/profiles/system --set "$store_path"
    sudo "$store_path/bin/switch-to-configuration" switch
}
deploy() (
    local target="$1"; shift
    local out_link="$1"; shift
    log_step "deploying $out_link to $target"
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
    if [[ $# -gt 0 && ("$1" == "--verbose" || "$1" == "-v" || "$1" == "-vv") ]]; then
        verbose="--verbose"
        if [[ "$1" == "-vv" ]]; then
            set -x
        fi
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

    flake_update "$( [[ "$MODE" == "" ]] && echo "full" || echo "fast")"

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

if ! have nix || ! have nvd; then
    log_error "nvd not found"
    exec nix-shell -p nvd --command "$0" "$@"
fi

main "$@"; times; date
