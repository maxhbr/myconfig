{ pkgs, lib, ... }:
{ imports =
    [ ./sd-image-raspberrypi4.nix
      ../../modules/service.openssh.nix
      ../../roles/core.nix
      ../../secrets/common/wifi.QS3j.nix
      ../../modules/service.monitoring.nix
      ../../roles/headless.nix
    ];

  networking.hostName = "pi4";
  networking.hostId = "ac8edd7a";
}
