# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraConfig = "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";
  };
  environment.systemPackages = with pkgs; [
    pavucontrol pamix
  ];
  nixpkgs.config.pulseaudio = true;
}
