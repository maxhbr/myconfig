{ lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../modules
    # ../../secrets/common/wifi.home.nix
    ./raspicam.nix
    # NOTE: previously called `setupAsWireguardClient` here via a stale
    # `import ../lib.nix` path that no longer exists. The active wg
    # client setup for hosts in this repo is now option-based; enable
    # in the private repo with:
    #   myconfig.wireguard.wg0 = {
    #     enable           = true;
    #     privateKeySource = <path-to-age-encrypted-private-key>;
    #   };
    # See modules/myconfig.wireguard/README.md for details.
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
