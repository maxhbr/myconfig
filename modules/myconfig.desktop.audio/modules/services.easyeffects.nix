# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EasyEffects (https://github.com/wwmm/easyeffects) — audio effects
# pipeline (equalizer, compressor, rnnoise, …) for PipeWire.
#
# Keys off PipeWire: the module is a no-op unless `services.pipewire.enable`
# is set (which is the case on every host importing `myconfig.desktop.audio`).
#
# The home-manager `services.easyeffects` module (upstream) installs the
# package and registers a systemd user service that is wanted by
# `graphical-session.target` — the same autostart anchor used by voxtype
# and clipboard-sync — so EasyEffects starts automatically with the
# desktop session (niri, labwc, …) and quits with it.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myconfig.desktop.audio.easyeffects;
in
{
  options.myconfig.desktop.audio.easyeffects = with lib; {
    enable = mkEnableOption "EasyEffects (PipeWire audio effects)" // {
      default = config.services.pipewire.enable;
    };
  };

  config = lib.mkIf cfg.enable {
    # The EasyEffects daemon stores its settings via GSettings/dconf.
    programs.dconf.enable = lib.mkDefault true;

    home-manager.sharedModules = [
      {
        services.easyeffects.enable = true;
      }
    ];
  };
}
