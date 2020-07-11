{ lib, ... }:
{ imports =
    [ ./rpi3
      ../../../modules/service.openssh.nix
      ../../../roles/core.nix
      ../../../modules/kiosk/desktop.X.kiosk.nix
    ];

  networking.hostName = "pi3a";
  networking.hostId = "78acddde";
  networking.networkmanager.enable = lib.mkForce false;

  # bzip2 compression takes loads of time with emulation, skip it. Enable this if you're low
  # on space.
  sdImage.compressImage = false;
}
