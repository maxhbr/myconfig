# based on:
# <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-raspberrypi4.nix>
# this mainly removes the `installation-device` part
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/profiles/base.nix")
    (modulesPath + "/installer/sd-card/sd-image.nix")
  ];

  nixpkgs.system = "aarch64-linux";

  boot.consoleLogLevel = lib.mkDefault 7;
  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 4;
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_rpi4;

  hardware.enableRedistributableFirmware = true;

  sdImage = {
    firmwareSize = 1024;
    compressImage = false;

    firmwarePartitionName = "NIXOS_BOOT";
    # This is a hack to avoid replicating config.txt from boot.loader.raspberryPi
    populateFirmwareCommands =
      "${config.system.build.installBootLoader} ${config.system.build.toplevel} -d ./firmware";

    # As the boot process is done entirely in the firmware partition.
    populateRootCommands =
      "touch files/touched"; # See: https://github.com/NixOS/nixpkgs/pull/93175
  };

  fileSystems."/boot/firmware" = {
    # This effectively "renames" the loaOf entry set in sd-image.nix
    mountPoint = "/boot";
    neededForBoot = true;
  };
}
