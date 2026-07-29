# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Hardware / SD-image configuration for the FriendlyElec NanoPC-T6
# (Rockchip RK3588, aarch64).
#
# Follows ./install-nixos-nanopc-t6.md.
#
# The NanoPC-T6 cannot boot directly from NVMe/USB: the board-specific
# Rockchip U-Boot and /boot must live on eMMC or microSD.  We build a generic
# aarch64 SD image (empty FAT "FIRMWARE" partition + ext4 root) and additionally
# `dd` the NanoPC-T6 U-Boot (`u-boot-rockchip.bin`) into the raw gap in front of
# the first partition via `sdImage.postBuildCommands`, so the produced image is
# directly flashable and bootable with a single `dd` to microSD or eMMC.
#
# Once booted, the system can be re-installed / moved to eMMC (with root
# optionally on NVMe while /boot stays on eMMC/microSD) as described in the
# install guide.
{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/profiles/base.nix")
    (modulesPath + "/installer/sd-card/sd-image.nix")
  ];

  nixpkgs.hostPlatform.system = "aarch64-linux";

  # U-Boot on the NanoPC-T6 loads the kernel/initrd via an extlinux.conf that
  # it reads from the (bootable) ext4 root partition, populated below.
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.consoleLogLevel = lib.mkDefault 7;

  # RK3588 debug UART is ttyS2 at 1500000 baud (see nixos-hardware rockchip
  # and the FriendlyElec wiki).
  boot.kernelParams = [
    "console=ttyS2,1500000n8"
    "console=tty0"
  ];

  # TODO: verify/adjust once real hardware has been scanned with
  # `nixos-generate-config`.
  boot.initrd.availableKernelModules = [
    "nvme"
    "usb_storage"
    "sdhci_pci"
    "mmc_block"
  ];

  # base.nix enables ZFS in supportedFilesystems, but the ZFS kernel module is
  # broken against `linuxPackages_latest` (which the install guide requires for
  # RK3588 support). Disable ZFS so evaluation/build succeeds.
  boot.supportedFilesystems.zfs = lib.mkForce false;

  # Enables DHCP on each ethernet and wireless interface.
  networking.useDHCP = lib.mkDefault true;

  sdImage = {
    # The FAT "FIRMWARE" partition is only meaningful for the Raspberry Pi
    # family; the NanoPC-T6 U-Boot lives in the raw gap in front of the first
    # partition (see postBuildCommands).  Leave the partition empty.
    populateFirmwareCommands = "";

    # Populate /boot on the ext4 root partition with the extlinux config
    # (and device trees) that U-Boot reads.
    populateRootCommands = ''
      mkdir -p ./files/boot
      ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
    '';

    # Ship the image uncompressed so it can be `dd`-ed directly.
    compressImage = false;

    # Reserve a 32 MiB gap in front of the first partition so the (multi-MiB)
    # RK3588 U-Boot image fits without clobbering the FAT partition.  This
    # mirrors the 32 MiB start of the first partition in the install guide's
    # eMMC layout.
    firmwarePartitionOffset = 32;

    # Fuse the NanoPC-T6 U-Boot into the image.  `u-boot-rockchip.bin` bundles
    # TPL/SPL + ATF + U-Boot and is written at sector offset 64 (32 KiB), per
    # the install guide and the Rockchip boot ROM layout.
    postBuildCommands = ''
      dd if=${pkgs.ubootNanoPCT6}/u-boot-rockchip.bin of=$img conv=notrunc,fsync bs=512 seek=64
    '';
  };
}
