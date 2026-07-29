# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Placeholder hardware configuration for the FriendlyElec NanoPC-T6.
#
# This board has NOT been provisioned yet, so no real disk UUIDs, MAC
# addresses or scanned kernel modules are known. Once the system boots from
# microSD/eMMC, regenerate this file on the target with:
#
#     nixos-generate-config --root /mnt
#
# and replace the TODO placeholders below with the real values from
# `lsblk -f`.
#
# Per the install guide, the root filesystem is created with a "nixos" label
# (`mkfs.ext4 -L nixos`). The by-label device below is therefore accurate for
# the documented single-partition eMMC layout; the optional separate /boot on
# eMMC/microSD (with root on NVMe) is left as a TODO.
{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  # TODO: verify/adjust once real hardware has been scanned.
  boot.initrd.availableKernelModules = [
    "nvme"
    "usb_storage"
    "sdhci_pci"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # Single ext4 root labelled "nixos" (see install guide, step 5).
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  # TODO (optional NVMe layout): if /boot is kept on eMMC/microSD while root
  # lives on NVMe, add the real by-uuid entries here, e.g.:
  #   fileSystems."/boot" = {
  #     device = "/dev/disk/by-uuid/TODO-BOOT-UUID";
  #     fsType = "ext4";
  #   };
  #   fileSystems."/" = {
  #     device = "/dev/disk/by-uuid/TODO-NVME-ROOT-UUID";
  #     fsType = "ext4";
  #   };

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
}
