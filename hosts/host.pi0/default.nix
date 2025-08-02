{ lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../modules
    # ../../secrets/common/wifi.home.nix
    ./raspicam.nix
  ]
  ++ (with (import ../lib.nix); [ (setupAsWireguardClient "10.199.199.9") ]);

  config = {
    networking.hostName = "pi0";
    networking.hostId = "a88cddde";

    swapDevices = [
      {
        device = "/swapfile";
        priority = 0;
        size = 1024;
      }
    ];
  };
}
