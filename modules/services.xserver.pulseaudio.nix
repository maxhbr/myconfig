# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
let
  user = config.myconfig.user;

  pactl-monitor = pkgs.writeShellScriptBin "pactl-monitor" ''
    set -e
    pactl_monitor_file=/tmp/.pactl_monitor_file
    if [[ ! -f $pactl_monitor_file ]]; then
      touch $pactl_monitor_file
      ${config.hardware.pulseaudio.package}/bin/pactl load-module module-loopback latency_msec=1
      echo "enable loopback"
    else
      rm $pactl_monitor_file
      ${config.hardware.pulseaudio.package}/bin/pactl unload-module module-loopback || true
      echo "disable disable"
    fi
  '';
in {
  config = (lib.mkIf config.hardware.pulseaudio.enable {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [ pavucontrol pamix pactl-monitor noisetorch ];
    };

    hardware.pulseaudio = {
      package = pkgs.pulseaudioFull;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      extraConfig =
        "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";
    };
    nixpkgs.config.pulseaudio = true;
  });
}
