. ./common.sh

realize() {
    local targetHost="$1"
    shift

    logH1 "deploy" "targetHost=$targetHost args=$args jobCountArgs=$jobCountArgs"

    if nix ping-store --store ssh://builder.192.168.178.90; then
        jobCountArgs="-j0"
    else
        jobCountArgs=""
    fi

    (set -x;
     nixops deploy \
            $NIX_PATH_ARGS \
            $jobCountArgs \
            --show-trace --keep-failed \
            --fallback \
            --deployment $NIXOPS_DEPLOYMENT \
            "$@" \
            --include "$targetHost")
}

export -f realize
