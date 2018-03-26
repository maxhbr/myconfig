# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  environment.systemPackages = with pkgs; [
    pavucontrol pamix
  ];
  nixpkgs.config.pulseaudio = true;
}
