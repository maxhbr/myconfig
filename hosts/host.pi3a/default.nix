{ lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./raspicam.nix
    # ./gstreamer.nix
  ];

  networking.hostName = "pi3a";
  networking.hostId = "78acddde";

  swapDevices = [{
    device = "/swapfile";
    priority = 0;
    size = 1024;
  }];
}
