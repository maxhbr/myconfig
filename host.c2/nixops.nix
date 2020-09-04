with (import ../lib.nix);
mkHostNixops "c2" ({ lib, ... }: { imports = [ (fixIp "c2" "eth0") ]; })
