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
    (writeShellScriptBin "mb660_switch_profile" ''
    export PATH=$PATH:${pulseaudio}/bin:${bash}/bin
    ${./bin/switch_sennheiser_profile}
    '')
  ];
  nixpkgs.config.pulseaudio = true;
}
