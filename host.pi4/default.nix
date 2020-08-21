{ ... }:
{ imports =
    [ ./hardware-configuration.nix
      ../../modules/service.openssh.nix
      ../../roles/core.nix
      ../../modules/service.monitoring.nix
      ../../roles/headless.nix
      ../../modules/service.syncthing.nix
    ] ++
    (with (import ../lib.nix);
      [ (setupAsWireguardClient "10.199.199.8")
      ]
    );

  networking.hostName = "pi4";
  networking.hostId = "ac8edd7a";


  swapDevices =
    [{device = "/swapfile";
      priority = 0;
      size = 4096;
    }];
}
