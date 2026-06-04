# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Base configuration to run the selected Wayland compositors via uwsm
# (Universal Wayland Session Manager). uwsm wraps a standalone compositor with
# a set of systemd units on the fly, binding it into
# `graphical-session-pre.target`, `graphical-session.target` and
# `xdg-desktop-autostart.target`.
#
# The individual compositor wrappers (e.g. niri) register themselves via
# `programs.uwsm.waylandCompositors.<name>` and build their greetd session
# command with the `myconfig.desktop.wayland.uwsm.mkSessionCommand` helper.
#
# Compare against:
#   https://raw.githubusercontent.com/Swarsel/.dotfiles/refs/heads/main/modules/nixos/client/uwsm.nix
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig;
  uwsmCfg = cfg.desktop.wayland.uwsm;
in
{
  options.myconfig = with lib; {
    desktop.wayland.uwsm = {
      enable = mkEnableOption "running Wayland compositors via uwsm";

      package = mkPackageOption pkgs "uwsm" { };

      mkSessionCommand = mkOption {
        type = types.functionTo types.str;
        readOnly = true;
        description = lib.mdDoc ''
          Helper used by individual compositor wrappers to build a greetd
          session command that launches the compositor through uwsm.

          When uwsm is disabled this is the identity function returning the
          plain compositor command, so the wrappers do not need to branch.

          Takes an attribute set:
          - `binPath`: path/command of the compositor binary to launch
          - `extraArgs` (optional): list of extra arguments
        '';
      };
    };
  };

  config = lib.mkMerge [
    {
      # Always provide the helper so compositor wrappers can call it
      # unconditionally. It only routes through uwsm when uwsm is enabled.
      myconfig.desktop.wayland.uwsm.mkSessionCommand =
        {
          binPath,
          extraArgs ? [ ],
        }:
        if uwsmCfg.enable then
          "${lib.getExe uwsmCfg.package} start -F -- ${binPath} ${lib.escapeShellArgs extraArgs}"
        else
          "${binPath} ${lib.escapeShellArgs extraArgs}";
    }

    (lib.mkIf (cfg.desktop.wayland.enable && uwsmCfg.enable) {
      programs.uwsm = {
        enable = true;
        package = uwsmCfg.package;
      };
    })
  ];
}
