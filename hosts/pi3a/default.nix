{ ... }:
{ imports =
    [ ./hardware-configuration.nix
      ../../modules/service.openssh.nix
      ../../roles/core.nix
      ../../secrets/common/wifi.QS3j.nix
      # ./raspicam.nix
    ];

  networking.hostName = "pi3a";
  networking.hostId = "78acddde";
}
