{ lib, ... }: {
  imports =
    [ ./hardware-configuration.nix ../modules ../secrets/common/wifi.home.nix ]
    ++ (with (import ../lib.nix); [ (setupAsWireguardClient "10.199.199.8") ]);

  config = {
    myconfig = { headless.enable = true; };
    networking.hostName = "pi4";
    networking.hostId = "ac8edd7a";

    services.vsftpd.enable = lib.mkForce false; # fails to build on arm

    swapDevices = [{
      device = "/swapfile";
      priority = 0;
      size = 4096;
    }];
  };
}
