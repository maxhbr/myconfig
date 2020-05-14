. ../common.sh

realize() {
    local targetHost="$1"
    shift

    logH1 "deploy" "targetHost=$targetHost args=$args"
    (set -x;
     nixops deploy \
            $NIX_PATH_ARGS \
            --show-trace --keep-failed \
            --fallback \
            --deployment $NIXOPS_DEPLOYMENT \
            "$@" \
            --include "$targetHost")
}

export -f realize
