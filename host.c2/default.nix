{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../modules/service.openssh.nix
    ../../roles/core.nix
    ../../secrets/common/wifi.home.nix
  ];

  networking.hostName = "c2";
  networking.hostId = "78acdded";
  sdImage.compressImage = false;
}
