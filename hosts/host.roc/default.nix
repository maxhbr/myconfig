# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Firefly ROC-RK3568-PC (aarch64-linux) bootstrap skeleton.
#
# See ./README.md for the full installation plan.  This is a minimal,
# behaviour-light configuration meant to be fleshed out once the hardware
# has actually booted NixOS.  Anything that depends on real hardware or
# provisioning data is marked with TODO below.
{
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
  ];

  config = {
    # The ROC-RK3568-PC is brought up through a board-specific EDK2/UEFI
    # image, so it presents itself as a normal UEFI AArch64 system.
    boot.loader = {
      systemd-boot.enable = true;
      # Embedded UEFI implementations do not always provide reliable
      # persistent EFI variables; install the removable-media fallback
      # loader instead.  mkForce overrides the shared systemd-boot module
      # (modules/boot.loader.systemd-boot.nix) which defaults this to true.
      efi.canTouchEfiVariables = lib.mkForce false;
      grub.enable = false;
    };

    # Prefer a recent kernel while RK3568 mainline support is being
    # evaluated (see README.md).
    boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

    # TODO: enable and pin the board device tree only after confirming the
    # DTB exists in the selected kernel package (see README.md):
    #   hardware.deviceTree = {
    #     enable = true;
    #     name = "rockchip/rk3568-firefly-roc-pc.dtb";
    #   };

    myconfig = {
      headless.enable = true;
    };

    networking.hostName = "roc";
    # TODO: replace with a real, unique 8-hex-digit host id once the machine
    # is provisioned (e.g. `head -c 8 /etc/machine-id`).
    networking.hostId = "0c000000";

    # Two Gigabit Ethernet ports; use DHCP until the interface names and
    # MAC/PHY combinations have been confirmed from the boot log.
    networking.useDHCP = lib.mkDefault true;

    services.openssh.enable = true;

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
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "25.05"; # Did you read the comment?
  };
}
