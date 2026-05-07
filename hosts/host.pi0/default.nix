{ lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../modules
    # ../../secrets/common/wifi.home.nix
    ./raspicam.nix
    # NOTE: previously called `setupAsWireguardClient` here via a stale
    # `import ../lib.nix` path that no longer exists. The active wg client
    # setup for hosts in this repo lives in the private repo and is invoked
    # as `myconfig.metadatalib.setupAsWireguardClient "wg0" <agePrivKey>`.
  ];

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
