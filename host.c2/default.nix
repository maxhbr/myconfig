{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../modules/service.openssh.nix
    ../../roles/core.nix
    ../../secrets/common/wifi.QS3j.nix
  ];

  networking.hostName = "c2";
  networking.hostId = "78acdded";
  sdImage.compressImage = false;
}
