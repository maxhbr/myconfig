{ lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    # ./raspicam.nix
    # ./gstreamer.nix
    ({pkgs, ...}: {
       services.cage = {
        enable = true;
        program = "${pkgs.foot}/bin/foot";
        user = "some_user";
      };

      # wait for network and DNS
      systemd.services."cage-tty1".after = [
        "network-online.target"
        "systemd-resolved.service"
      ];
    })
  ];

  config = {
    networking.hostName = "pi3a";
    networking.hostId = "78acddde";

    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
        };
      };
    };


    swapDevices = [{
      device = "/swapfile";
      priority = 0;
      size = 1024;
    }];
  };
}
