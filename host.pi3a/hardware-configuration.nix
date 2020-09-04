# based on:
# <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-raspberrypi.nix>
{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/profiles/base.nix>
    # <nixpkgs/nixos/modules/profiles/installation-device.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/sd-image.nix>
  ];

  nixpkgs.system = "aarch64-linux";

  boot.consoleLogLevel = lib.mkDefault 7;
  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 3;
  # boot.loader.raspberryPi.uboot.enable = true;
  boot.loader.raspberryPi.firmwareConfig = ''
    gpu_mem=72
  '';
  # # boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_rpi3;

  hardware.enableRedistributableFirmware = true;

  # The serial ports listed here are:
  # Also increase the amount of CMA to ensure the virtual console on the RPi3 works.
  boot.kernelParams = [ "cma=32M" "console=tty0" ];

  boot.initrd.availableKernelModules = [
    # Allows early (earlier) modesetting for the Raspberry Pi
    "vc4"
    "bcm2835_dma"
    "i2c_bcm2835"
  ];

  sdImage = {
    firmwareSize = 1024;
    compressImage = false;
    populateFirmwareCommands = let
      configTxt = pkgs.writeText "config.txt" ''
        kernel=u-boot-rpi3.bin

        gpu_mem=72

        # Boot in 64-bit mode.
        arm_control=0x200

        # U-Boot used to need this to work, regardless of whether UART is actually used or not.
        # TODO: check when/if this can be removed.
        enable_uart=1

        # Prevent the firmware from smashing the framebuffer setup done by the mainline kernel
        # when attempting to show low-voltage or overtemperature warnings.
        avoid_warnings=1
      '';
    in ''
      (cd ${pkgs.raspberrypifw}/share/raspberrypi/boot && cp bootcode.bin fixup*.dat start*.elf $NIX_BUILD_TOP/firmware/)
      cp ${pkgs.ubootRaspberryPi3_64bit}/u-boot.bin firmware/u-boot-rpi3.bin
      cp ${configTxt} firmware/config.txt
      ${config.system.build.installBootLoader} ${config.system.build.toplevel} -d ./firmware
    '';
    # populateRootCommands = ''
    #   mkdir -p ./files/boot
    #   ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
    # '';
    firmwarePartitionName = "NIXOS_BOOT";
    # This is a hack to avoid replicating config.txt from boot.loader.raspberryPi

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
