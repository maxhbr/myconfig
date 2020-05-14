. ./common.sh

prepare_setup_nixops_deployment() {
    nixops info -d $NIXOPS_DEPLOYMENT || nixops create -d $NIXOPS_DEPLOYMENT "$myconfigDir/nixops/nixops.nix"
}

prepare_update_hostid_file() {
    if [[ ! -f "/etc/nixos/hostid" ]]; then
        echo "set hostid:"
        cksum /etc/machine-id |
            while read c rest; do printf "%x" $c; done |
            sudo tee "/etc/nixos/hostid"
    fi
}
prepare_update_hardware_configuration() {
    local nixosConfig="$myconfigDir/nixos/host-$(hostname)"
    if [[ -d "$nixosConfig" ]]; then
        if [[ -f "/etc/nixos/hardware-configuration.nix" ]]; then
            if ! cmp "/etc/nixos/hardware-configuration.nix" "$nixosConfig/hardware-configuration.nix" >/dev/null 2>&1; then
                logH1 "update" "$nixosConfig/hardware-configuration.nix"
                cat "/etc/nixos/hardware-configuration.nix" | tee "$nixosConfig/hardware-configuration.nix"
            fi
        fi
    fi
}
prepare_create_nix_store_key() {
    # https://github.com/NixOS/nix/issues/2330#issuecomment-410505837
    if [[ ! -f ~/.config/nix/pk ]]; then
        logH1 "generate" "~/.config/nix/pk and ~/.config/nix/sk"
        mkdir -p ~/.config/nix/
        nix-store --generate-binary-cache-key machine.$(hostname) ~/.config/nix/sk ~/.config/nix/pk
        cat ~/.config/nix/pk
    fi
    # https://blog.joel.mx/posts/how-to-use-nix-copy-closure-step-by-step
    if [[ ! -f /etc/nix/signing-key.sec ]]; then
        logH1 "generate" "signing-key at /etc/nix/signing-key.sec and /etc/nix/signing-key.pub"
        (umask 277 && sudo openssl genrsa -out /etc/nix/signing-key.sec 2048)
        sudo openssl rsa -in /etc/nix/signing-key.sec -pubout | sudo tee /etc/nix/signing-key.pub
    fi
}
prepare_load_prefetches() {
    logH1 "prefetch" "$myconfigDir/prefetches/"
    if [[ -d "$myconfigDir/prefetches/" ]]; then
        find "$myconfigDir/prefetches/" \
            -not -path '*/\.*' \
            -type f \
            -exec  nix-prefetch-url "file://{}" \;
    else
        echo "no folder ./prefetches"
    fi
}
prepare() {
    if [[ -f /etc/nixos/configuration.nix ]]; then
        echo "/etc/nixos/configuration.nix should not exist"
        exit 1
    fi
    prepare_setup_nixops_deployment
    prepare_update_hostid_file
    prepare_update_hardware_configuration
    prepare_create_nix_store_key

    prepare_load_prefetches
}

export -f prepare
