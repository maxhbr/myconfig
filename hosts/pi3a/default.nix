with (import ../lib.nix);
{ ... }:
{ imports =
    [ ./hardware-configuration.nix
      ../../modules/service.openssh.nix
      ../../roles/core.nix
      ../../secrets/common/wifi.QS3j.nix
      ./raspicam.nix
      (setupAsWireguardClient "10.199.199.7")
    ];

  networking.hostName = "pi3a";
  networking.hostId = "78acddde";

  swapDevices = [{
    device = "/swapfile";
    priority = 0;
    size = 1024;
  }];
}
