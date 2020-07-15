{ lib, ... }:
{ imports =
    [ <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-aarch64.nix>
      ../../modules/service.openssh.nix
      ../../roles/core.nix
      ../../secrets/common/wifi.QS3j.nix
    ];

  networking.hostName = "pi3a";
  networking.hostId = "78acddde";
  sdImage.compressImage = false;
}
