{ lib, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.raspberry-pi-3
  ];

  config = {
    networking.hostName = "pi3a";
    networking.hostId = "78acddde";
    networking.wireless.enable = lib.mkForce false;

    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
        };
      };
    };

    services.greetd.enable = lib.mkForce false;

    swapDevices = [
      {
        device = "/swapfile";
        priority = 0;
        size = 1024;
      }
    ];
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "23.05"; # Did you read the comment?
  };
}
