with (import ../lib.nix);
mkHostNixops "x1extremeG2" ({ lib, ... }: {
  config = { deployment.targetHost = lib.mkDefault "10.199.199.2"; };
  imports = [
    ../secrets/common/wifi.home.nix
    ../secrets/common/wifi.Hotspot.nix
    (deployWireguardKeys "x1extremeG2")
    (deploySSHUserKeys "x1extremeG2" "rsa")
    (deploySSHUserKeys "x1extremeG2" "ed25519")
    (deploySSHUserKeys "x1extremeG2" "dsa")
    (deploySSHUserKeys "x1extremeG2" "ecdsa")
    (setupNixServe [
      (getSecret "workstation" "ssh/id_rsa.pub")
      (getSecret "vserver" "ssh/id_rsa.pub")
    ])
    (setupBuildSlave (getSecretNoNewline "workstation" "ip") 2 [
      "x86_64-linux"
      "aarch64-linux"
      "armv6l-linux"
    ] (getSecret "x1extremeG2" "ssh/id_ed25519")
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDtqOcfT93S4gCROVvhTxB3Imp4bnbrtzQRnF9oRLTDs")
    (setupBuildSlave (getSecretNoNewline "pi4" "ip") 0 [ "aarch64-linux" ]
      (getSecret "x1extremeG2" "ssh/id_ed25519")
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPYDCNFSDUaE1R0HUeikFcLK7Dp1w5CPKGWcXICq3iQd")
    (setupSyncthing "x1extremeG2" ((mkSyncthingDevice "nas" false)
      // (mkSyncthingDevice "workstation" false)
      // (mkSyncthingDevice "vserver" false)
      // (import ../secrets/common/syncthing.SM-G960F.nix)
      // (import ../secrets/common/syncthing.Pixel5.nix)) {
        "/home/mhuber/Sync" = {
          id = "sync";
          devices = [ "nas" "workstation" "vserver" "Pixel5" ];
          type = "sendreceive";
        };
        "/home/mhuber/Bilder/00-galerie" = {
          id = "00-galerie";
          devices = [ "nas" ];
          type = "sendonly";
        };
        "/home/mhuber/syncthing/SM-G960F" = {
          id = "Voo3Hidi"; # random
          devices = [ "SM-G960F" ];
          type = "receiveonly";
          versioning = {
            type = "simple";
            params.keep = "10";
          };
        };
        "/home/mhuber/syncthing/Pixel5" = {
          id = "Voo4Hidi"; # close to random
          devices = [ "Pixel5" ];
          type = "sendreceive";
          versioning = {
            type = "simple";
            params.keep = "10";
          };
        };
      })
  ];
})
