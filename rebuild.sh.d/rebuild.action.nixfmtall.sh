nixfmtall() {
    logH1 "nixfmt" ""
    time find "$myconfigDir" \
         -iname '*.nix' \
         -not -iname 'empty_nixos_config.nix' \
         -not -path '*nixpkgs/*' \
         -not -path '*/nixos-hardware/*' \
         $@ \
         -exec nixfmt {} \;
}

export -f nixfmtall
