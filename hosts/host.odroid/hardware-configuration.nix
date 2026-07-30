# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Hardware configuration for the Hardkernel ODROID-C2 (Amlogic S905, aarch64).
#
# Follows the NixOS wiki instructions:
#   https://nixos.wiki/wiki/NixOS_on_ARM/ODROID-C2
#
# The ODROID-C2 boots from a board-specific U-Boot (pkgs.ubootOdroidC2) that
# has to be written to fixed raw sector offsets on the boot medium.  We build a
# generic aarch64 SD image (MBR: FAT "FIRMWARE" + ext4 root) and additionally
# `dd` the U-Boot blobs into the gap in front of the first partition via
# `sdImage.postBuildCommands`, so the produced image is directly flashable and
# bootable with a single `dd`.
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

  hardware.enableRedistributableFirmware = true;

  # U-Boot on the ODROID-C2 loads the kernel/initrd via an extlinux.conf that
  # it reads from the (bootable) ext4 root partition, populated below.
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.consoleLogLevel = lib.mkDefault 7;

  # The ODROID-C2 serial console is the Amlogic UART (ttyAML0).  Per the wiki
  # only this console (plus the framebuffer tty0) should be on the kernel
  # command line - the Tegra/QEMU consoles from sd-image-aarch64.nix do not
  # apply here.
  #
  # `earlycon` is required to see *early* kernel output on this board: without
  # it, anything the kernel prints before the real `meson` 8250-style serial
  # driver probes is silently dropped, so an early-boot panic/oops resets the
  # board with *no* serial output at all (symptom: "reboot loop, never see any
  # log").  The bare `earlycon` form auto-configures from the DT `stdout-path`
  # (serial_AO on the meson-gxbb DTB), so no hard-coded MMIO address is needed.
  # This is a diagnostics-only change: it does not alter the boot chain, only
  # makes a pre-userspace failure visible so its root cause can be confirmed on
  # the console.
  boot.kernelParams = [
    "cma=32M"
    "earlycon"
    "console=ttyAML0,115200n8"
    "console=tty0"
  ];

  boot.initrd.availableKernelModules = [
    "mmc_block"
    "usbhid"
    "hid_generic"
    "dwmac_meson8b"
    "meson_gxbb_wdt"
  ];

  # Enables DHCP on each ethernet and wireless interface.
  networking.useDHCP = lib.mkDefault true;

  sdImage = {
    # The FAT "FIRMWARE" partition is only meaningful for the Raspberry Pi
    # family; the ODROID-C2 U-Boot lives in the raw gap in front of the first
    # partition (see postBuildCommands).  Leave the partition empty.
    populateFirmwareCommands = "";

    # Populate /boot on the ext4 root partition with the extlinux config that
    # U-Boot reads.
    populateRootCommands = ''
      mkdir -p ./files/boot
      ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
    '';

    # Ship the image uncompressed so it can be `dd`-ed directly.
    compressImage = false;

    # Fuse the ODROID-C2 U-Boot into the image.  Offsets per the flashing
    # instructions in pkgs.ubootOdroidC2 (Amlogic S905 boot ROM layout):
    #   bl1.bin.hardkernel : first 442 bytes, then from sector 1
    #   u-boot.gxbb        : from sector 97
    # These all fit comfortably inside the default 8 MiB firmwarePartitionOffset
    # gap in front of the first partition.
    postBuildCommands = ''
      uboot=${pkgs.ubootOdroidC2}
      dd if=$uboot/bl1.bin.hardkernel of=$img conv=notrunc bs=1   count=442
      dd if=$uboot/bl1.bin.hardkernel of=$img conv=notrunc bs=512 skip=1 seek=1
      dd if=$uboot/u-boot.gxbb        of=$img conv=notrunc bs=512 seek=97
    '';
  };

  # https://github.com/NixOS/nixpkgs/issues/154163 - the meson dtbs reference
  # modules that are not always present; allow the modules closure to build
  # with missing entries (mirrors the r6c aarch64 host).
  nixpkgs.overlays = [
    (final: super: {
      makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; });
    })
  ];
}
