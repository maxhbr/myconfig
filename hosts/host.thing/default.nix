# Copyright 2016-2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ../../hardware/efi.nix
    ../../hardware/btrfs.nix
    inputs.nixos-hardware.nixosModules.framework-desktop-amd-ai-max-300-series
    (myconfig.metadatalib.fixIp "enp191s0")
    (myconfig.metadatalib.setupAsBuildMachine [
      myconfig.metadatalib.get.hosts.p14.pubkeys."id_ed25519_no_pw.pub"
    ])
    { environment.systemPackages = with pkgs; [ linuxPackages.usbip ]; }
    {
      # programs.mosh.enable = lib.mkDefault true;
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.allowedTCPPorts = [ 22022 ];
      networking.firewall.allowedUDPPorts = [ 22022 ];
    }
    ./ai.thing.nix
    ./Radeon8060S.nix
    {
      services.earlyoom.enable = lib.mkForce false;
    }
  ];

  config = {
    networking.hostName = "thing";
    networking.hostId = "3e7c8a1f";
    networking.useDHCP = false;

    myconfig = {
      persistence.impermanence = {
        enable = true;
        soft_permanence_for_boot = false;
        tmpfs_size = "20%";
        btrfs_device = "/dev/mapper/enc-pv";
      };
      headless.enable = true;
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          selectedSessions = [
            "niri"
            "labwc"
          ];
        };
        imagework.enable = true; # https://github.com/NixOS/nixpkgs/issues/425306
        imagework.myphoto.enable = true;
      };
      virtualisation.enable = true;
    };

    virtualisation = {
      # docker.enable = true;
      podman.enable = true;
      oci-containers = {
        backend = "podman";
      };
      # virtualbox.host.enable = true;
      # lxc.enable = true;
      # libvirtd.enable = true;
    };
    services.hardware.bolt.enable = true;

    hardware.enableRedistributableFirmware = true;

    boot = {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };
      initrd = {
        supportedFilesystems = [
          "btrfs"
          "luks"
          "nfs"
        ];
        kernelModules = [ "nfs" ];
      };
    };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "25.05"; # Did you read the comment?
  };
}
