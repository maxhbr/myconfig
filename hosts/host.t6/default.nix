# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# FriendlyElec NanoPC-T6 (Rockchip RK3588, aarch64-linux).
#
# See ./README.md for the full installation plan.  This is a minimal,
# behaviour-light configuration meant to be fleshed out once the hardware
# has actually booted NixOS.  Anything that depends on real hardware or
# provisioning data is marked with TODO below.
{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
  ];

  config = {
    # The NanoPC-T6 is brought up through a board-specific EDK2/UEFI
    # image from edk2-porting/edk2-rk3588, so it presents itself as a
    # normal UEFI AArch64 system.
    boot.loader = {
      systemd-boot.enable = true;
      # Embedded UEFI implementations do not always provide reliable
      # persistent EFI variables; install the removable-media fallback
      # loader instead.  mkForce overrides the shared systemd-boot module
      # (modules/boot.loader.systemd-boot.nix) which defaults this to true.
      efi.canTouchEfiVariables = lib.mkForce false;
      grub.enable = false;
    };

    # Prefer a recent kernel while RK3588 mainline support is being
    # evaluated (see README.md).
    boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

    # RK3588 debug UART is ttyS2 at 1500000 baud (see nixos-hardware rockchip
    # and the FriendlyElec wiki).
    boot.consoleLogLevel = lib.mkDefault 7;
    boot.kernelParams = [
      "console=ttyS2,1500000n8"
      "console=tty0"
    ];

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
    # TODO: replace with a real, unique 8-hex-digit host id once the machine
    # is provisioned (e.g. `head -c 8 /etc/machine-id`).
    networking.hostId = "74366e61";

    networking.networkmanager.enable = true;

    services.openssh.enable = true;

    swapDevices = [
      {
        device = "/swapfile";
        priority = 0;
        size = 4096;
      }
    ];

    # https://github.com/NixOS/nixpkgs/issues/154163
    # Some Rockchip/aarch64 kernels are missing modules referenced by the
    # module closure; allow them to be missing so the image still builds.
    nixpkgs.overlays = [
      (final: super: {
        makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; });
      })
    ];

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It's perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "25.11"; # Did you read the comment?
  };
}
