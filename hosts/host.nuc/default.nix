# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  imports = [
    ./smart-home
    ./observability.nix
    ./services.caddy.nix
    ./services.dependency-track.nix
    ./backup-hdd.nix
    ./hardware-configuration.nix
    ../../hardware/grub.nix
    ../../hardware/btrfs.nix
    {
      boot.initrd.supportedFilesystems = [
        "btrfs"
        "luks"
      ];
      services.btrfs.autoScrub = {
        enable = true;
      };
    }
    (myconfig.metadatalib.fixIp "enp0s20u2")
    {
      # programs.mosh.enable = lib.mkDefault true;
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.allowedTCPPorts = [ 22022 ];
      networking.firewall.allowedUDPPorts = [ 22022 ];
    }
    ../shared.deployedServices.nix
  ];
  config = {
    myconfig = {
      desktop.enable = false;
      headless.enable = true;
      smart-home.enable = true;
      deployedServices.configureCaddy = true;
      # Podman is needed for the Dependency-Track containers (see
      # services.dependency-track.nix). `myconfig.virtualisation.enable`
      # turns on podman; the explicit backend selects podman for oci-containers.
      virtualisation.enable = true;
    };

    virtualisation = {
      podman.enable = true;
      oci-containers.backend = "podman";
    };

    networking.hostName = "nuc";
    networking.hostId = "29d93123";

    # nuc was never assigned a stateVersion (the warning defaulted to the
    # current release). Set to the most recent NixOS version per user request.
    system.stateVersion = lib.mkForce "26.11";
  };
}
