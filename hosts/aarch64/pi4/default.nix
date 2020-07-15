{ pkgs, lib, ... }:
{ imports =
    [ <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-raspberrypi4.nix>
      ../../../modules/service.openssh.nix
      ../../../roles/core.nix
      ../../../secrets/common/wifi.QS3j.nix
      # testing:
      ../../../modules/service.monitoring.nix
      ../../../roles/headless.nix
    ];

  networking.hostName = "pi4";
  networking.hostId = "ac8edd7a";

  # bzip2 compression takes loads of time with emulation, skip it. Enable this if you're low
  # on space.
  sdImage =
    { compressImage = false;
      firmwareSize = lib.mkForce 1024;

      # the make-ext4-fs.nix fails, if no files were writen in ./files
      # See: https://github.com/NixOS/nixpkgs/pull/93175
      populateRootCommands = "touch files/touched";
    };

  services.mingetty.autologinUser = lib.mkForce null;
}
