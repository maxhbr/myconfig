# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see: https://discourse.nixos.org/t/nvidia-optimus-via-prime-on-laptop-not-working/4124

{ config, lib, pkgs, ... }:
{
  hardware = {
    nvidia = {
      optimus_prime = {
        enable = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
      modesetting.enable = true;
    };
  };
  # services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.videoDrivers = [ "intel" ];
}
