with (import ../lib.nix);
mkHostNixops "pi3a" ({ lib, ... }: {
  imports = [ (fixIp "pi3a" "wlan0") (deployWireguardKeys "pi3a") ];
})
