. ./common.sh

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
    prepare_create_nix_store_key

    prepare_load_prefetches
}

export -f prepare
