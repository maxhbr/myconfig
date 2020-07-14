{ pkgs, lib, ... }:
{ imports =
    [ <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-raspberrypi4.nix>
      ../../../modules/service.openssh.nix
      ../../../roles/core.nix
      ../../../secrets/common/wifi.QS3j.nix
    ];

  networking.hostName = "pi4";
  networking.hostId = "ac8edd7a";

  # bzip2 compression takes loads of time with emulation, skip it. Enable this if you're low
  # on space.
  sdImage.compressImage = false;
}
