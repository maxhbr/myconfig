{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ../role.core
    ./raspicam.nix
    ../secrets/common/wifi.home.nix
  ] ++ (with (import ../lib.nix); [ (setupAsWireguardClient "10.199.199.7") ]);

  networking.hostName = "pi3a";
  networking.hostId = "78acddde";

  swapDevices = [{
    device = "/swapfile";
    priority = 0;
    size = 1024;
  }];
}
