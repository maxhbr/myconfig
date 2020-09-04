with (import ../lib.nix);
mkHostNixops "T470s" ({ ... }: { imports = [ (deployWireguardKeys "T470s") ]; })
