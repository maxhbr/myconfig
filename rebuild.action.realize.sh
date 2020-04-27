. ./common.sh

realize() {
    if [[ "$1" == "--fast" ]]; then
        args="--fast"
        shift
    else
        args="--upgrade"
    fi

    local NIXOS_REBUILD_CMD=${NIXOS_REBUILD_CMD:-switch}
    logH1 "nixos-rebuild" "\$NIXOS_REBUILD_CMD=$NIXOS_REBUILD_CMD \$args=$args"
    logINFO "nixStableChannel=$nixStableChannel"
    logINFO "$NIX_PATH_ARGS"
    time sudo \
         -E \
         --preserve-env=NIX_PATH \
         NIX_CURL_FLAGS='-sS' \
         nixos-rebuild \
         $NIX_PATH_ARGS \
         --show-trace --keep-failed \
         $args \
         --fallback \
         $NIXOS_REBUILD_CMD | sed -e 's/^/['"$args"'] /'
    if [[ "$NIXOS_REBUILD_CMD" == "switch" ]]; then
        logH1 "nix path-info" "-hS /run/current-system"
        nix path-info -hS /run/current-system
    fi
}

export -f realize
