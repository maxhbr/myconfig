{ lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../modules/service.openssh.nix
    ../../roles/core.nix
    # ../../secrets/common/wifi.QS3j.nix
    ./raspicam.nix
  ] ++ (with (import ../lib.nix); [ (setupAsWireguardClient "10.199.199.9") ]);

  config = {
    networking.hostName = "pi0";
    networking.hostId = "a88cddde";

    swapDevices = [{
      device = "/swapfile";
      priority = 0;
      size = 1024;
    }];
  };
}
