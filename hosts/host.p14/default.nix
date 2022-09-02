# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../hardware/efi.nix
    ../../hardware/nixos-hardware/common/pc/ssd
    {
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;

      networking.useDHCP = false;
      networking.interfaces.enp0s31f6.useDHCP = true;
      networking.interfaces.wlp3s0.useDHCP = true;
      networking.interfaces.wwp0s20f0u5c2.useDHCP = true;
    }
  ];

  config = {
    networking.hostName = "p14";
    networking.hostId = "98234324";
    myconfig = {
      desktop.enable = false;
      virtualisation.enable = false;
    };
    virtualisation.docker.enable = false;
    virtualisation.podman.enable = false;
    # virtualisation.libvirtd.enable = true;

    services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    boot.kernelPackages = lib.mkForce pkgs.linuxPackages_testing;

    hardware.enableRedistributableFirmware = true;

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "21.05"; # Did you read the comment?
  };
}
