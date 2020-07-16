{ ... }:
{ imports =
    [ ./hardware-configuration.nix
      ../../modules/service.openssh.nix
      ../../roles/core.nix
      ../../modules/service.monitoring.nix
      ../../roles/headless.nix
    ];

  networking.hostName = "pi4";
  networking.hostId = "ac8edd7a";
}
