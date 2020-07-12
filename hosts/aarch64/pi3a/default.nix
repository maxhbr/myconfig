{ lib, ... }:
{ imports =
    [ ../nixos-docker-sd-image-builder/config/rpi3
      ../../../modules/service.openssh.nix
      ../../../roles/core.nix
      ../../../secrets/common/wifi.QS3j.nix
    ];

  networking.hostName = "pi3a";
  networking.hostId = "78acddde";

  # bzip2 compression takes loads of time with emulation, skip it. Enable this if you're low
  # on space.
  sdImage.compressImage = false;
}
