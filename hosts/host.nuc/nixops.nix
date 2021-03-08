with (import ../lib.nix);
mkHostNixops "nuc" ({ lib, config, ... }: {
  config = { deployment.targetHost = lib.mkDefault "10.199.199.9"; };
  imports = [
    (fixIp "nuc" "enp2s0")
    (deployWireguardKeys "nuc")
    ../secrets/common/wifi.home.nix
    {
      nix.trustedBinaryCaches =
        [ ("ssh://nix-ssh@" + (getSecretNoNewline "workstation" "ip")) ];
    }
  ];
})
