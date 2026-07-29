# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# FriendlyElec NanoPC-T6 (Rockchip RK3588, aarch64).
# See ./install-nixos-nanopc-t6.md for the full install guide this config
# is based on.
{
  config,
  myconfig,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    {
      # UART serial console (RK3588 debug UART).
      environment.systemPackages = with pkgs; [ tio ];
    }
  ];

  config = {
    # The NanoPC-T6 cannot boot directly from NVMe/USB. U-Boot and /boot must
    # live on eMMC or microSD. We therefore use the generic extlinux-compatible
    # bootloader (written to /boot) instead of GRUB/systemd-boot; the
    # board-specific Rockchip U-Boot is flashed to the boot device out of band
    # (see the install guide).
    boot.loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };

    boot.kernelPackages = pkgs.linuxPackages_latest;

    # Device tree for the NanoPC-T6.
    # For the NanoPC-T6 LTS use "rockchip/rk3588-nanopc-t6-lts.dtb" instead,
    # if the kernel provides it.
    hardware.deviceTree.name = "rockchip/rk3588-nanopc-t6.dtb";

    hardware.enableRedistributableFirmware = true;

    myconfig = {
      desktop.enable = false;
      headless.enable = true;
    };

    networking.hostName = "t6";
    # TODO: replace with a real, stable host id (`head -c4 /dev/urandom | od -A none -t x4`)
    networking.hostId = "74366e61";

    networking.useDHCP = lib.mkDefault true;
    networking.networkmanager.enable = true;

    swapDevices = [
      {
        device = "/swapfile";
        priority = 0;
        size = 4096;
      }
    ];

    # https://github.com/NixOS/nixpkgs/issues/154163
    # https://github.com/NixOS/nixpkgs/issues/111683#issuecomment-968435872
    # https://github.com/NixOS/nixpkgs/issues/126755#issuecomment-869149243
    nixpkgs.overlays = [
      (final: super: {
        makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; });
      })
    ];

    # This value determines the NixOS release from which the default
    # settings for stateful data were taken. Leave at the release version of
    # the first install of this system.
    system.stateVersion = lib.mkForce "25.11";
  };
}
