with (import ../lib.nix);
mkHostNixops "nuc" ({ lib, ... }: {
  config = { deployment.targetHost = lib.mkDefault "10.199.199.9"; };
  imports = [
    (fixIp "nuc" "enp2s0")
    (deployWireguardKeys "nuc")
    ../secrets/common/wifi.home.nix
    {
      nix.trustedBinaryCaches =
        [ ("ssh://nix-ssh@" + (getSecretNoNewline "workstation" "ip")) ];
    }
    # (setupSyncthing "nuc" (
    #   (mkSyncthingDevice "x1extremeG2" true)
    #   // (mkSyncthingDevice "workstation" false)
    #   // (mkSyncthingDevice "vserver" false)
    #   // (import ../secrets/common/syncthing.Pixel5.nix)) {
    #   })
    {
      deployment.keys = {
        "nginx.crt" = {
          text = getSecret "nuc" "tls/nginx.crt";
          destDir = "/etc/tls";
          user = "nginx";
          group = "root";
          permissions = "0440";
        };
        "nginx.key" = {
          text = getSecret "nuc" "tls/nginx.key";
          destDir = "/etc/tls";
          user = "nginx";
          group = "root";
          permissions = "0440";
        };
      };
      # services.nginx.virtualHosts."nuc".listen = [
      #   {addr = "10.199.199.9"; port = 443; ssl = true;}
      #   {addr = (getSecretNoNewline "nuc" "ip"); port = 443; ssl = true;}
      # ];
    }
  ];
})
