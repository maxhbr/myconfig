# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

{
  options = {
    myconfig.roles.virtualization = {
      enable = lib.mkEnableOption "Virtualization role";
    };
    myconfig.roles.virtualization-docker = {
      enable = lib.mkEnableOption "Docker Virtualization role";
    };
    myconfig.roles.virtualization-vbox = {
      enable = lib.mkEnableOption "VBox Virtualization role";
    };
    myconfig.roles.virtualization-qemu = {
      enable = lib.mkEnableOption "Qemu Virtualization role";
    };
  };

  imports = [
################################################################################
    { # docker
      config = lib.mkIf config.myconfig.roles.virtualization-docker.enable {
        environment.systemPackages = with pkgs; [
          docker
          docker-machine
          # docker-gc
          python35Packages.docker_compose
        ];

        virtualisation.docker = {
            enable = true;
            extraOptions = "--data-root /home/docker";
            storageDriver = "overlay2";
            # socketActivation = false;
            autoPrune.enable = true;
            package = pkgs.unstable.docker-edge;
        };
      };
    }
################################################################################
    { # vbox
      config = lib.mkIf config.myconfig.roles.virtualization-vbox.enable {
        environment.systemPackages = with pkgs; [
          vagrant
        ];
        virtualisation.virtualbox.host.enable = true;
        # virtualisation.virtualbox.host.headless = true;
      };
    }
################################################################################
    { # qemu
      config = lib.mkIf config.myconfig.roles.virtualization-qemu.enable {
        environment.systemPackages = with pkgs; [
          qemu aqemu
        ];
      };
    }
  ];

################################################################################
  config = lib.mkIf config.myconfig.roles.virtualization.enable {
    myconfig.roles.virtualization-docker.enable = true;
    myconfig.roles.virtualization-vbox.enable = true;
    myconfig.roles.virtualization-qemu.enable = true;
  };
}
