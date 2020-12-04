with (import ../lib.nix);
mkHostNixops "nuc" ({ lib, ... }: {
  config = { deployment.targetHost = lib.mkDefault "10.199.199.9"; };
  imports = [
    (fixIp "nuc" "enp2s0")
    ../secrets/common/wifi.home.nix
    {
      nix.trustedBinaryCaches =
        [ ("ssh://nix-ssh@" + (getSecret "workstation" "ip")) ];
    }
    # (setupSyncthing "nuc" (
    #   (mkSyncthingDevice "x1extremeG2" true)
    #   // (mkSyncthingDevice "workstation" false)
    #   // (mkSyncthingDevice "vserver" false)
    #   // (import ../secrets/common/syncthing.Pixel5.nix)) {
    #   })
  ];
})
