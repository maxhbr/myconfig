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
# package and registers a systemd user service. Upstream wires that unit
# into `graphical-session.target` — the same autostart anchor used by
# voxtype and clipboard-sync — so it would start automatically with the
# desktop session (niri, labwc, …).
#
# This wrapper keeps the autostart off by default: `Install.WantedBy` is
# overridden to an empty list, so the unit stays installed and startable
# on demand (`systemctl --user start easyeffects`, or just launch the app)
# but nothing pulls it in at session start. Set `autostart = true` to
# restore upstream behavior.
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

    autostart = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Start the EasyEffects daemon automatically with the graphical
        session. When disabled (default), the systemd user service stays
        installed and can be started on demand via
        `systemctl --user start easyeffects` or by launching the app.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # The EasyEffects daemon stores its settings via GSettings/dconf.
    programs.dconf.enable = lib.mkDefault true;

    home-manager.sharedModules = [
      {
        services.easyeffects.enable = true;

        # Upstream wires the unit into `graphical-session.target`, which
        # autostarts it with the session. Drop the install trigger unless
        # autostart is explicitly requested; the unit remains startable
        # on demand.
        systemd.user.services.easyeffects.Install.WantedBy = lib.mkIf (!cfg.autostart) (lib.mkForce [ ]);
      }
    ];
  };
}
