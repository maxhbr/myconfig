{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ../modules
    ../role.headless
    ../secrets/common/wifi.home.nix
  ] ++ (with (import ../lib.nix); [ (setupAsWireguardClient "10.199.199.8") ]);

  networking.hostName = "pi4";
  networking.hostId = "ac8edd7a";

  swapDevices = [{
    device = "/swapfile";
    priority = 0;
    size = 4096;
  }];
}
