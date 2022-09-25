# Copyright 2017 Maximilian nuber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

let
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
  config = lib.mkIf config.hardware.pulseaudio.enable {
    home-manager.sharedModules =
      [{ home.packages = with pkgs; [ pactl-monitor ]; }];
    hardware.pulseaudio = {
      package = pkgs.pulseaudioFull;
      extraConfig =
        "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";
    };
    nixpkgs.config.pulseaudio = true;
  };
}
