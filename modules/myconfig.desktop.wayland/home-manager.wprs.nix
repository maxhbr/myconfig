# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# wprs: rootless remote desktop access for remote wayland applications
# (like xpra, but for wayland).
#
# - locally:  wprs <remote_host> run <application>
#             wprs <remote_host> attach / detach
# - remotely: systemctl --user start wprsd
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.desktop.wayland.wprs;
in
{
  options.myconfig.desktop.wayland.wprs = with lib; {
    enable = mkEnableOption "myconfig.desktop.wayland.wprs" // {
      default =
        config.myconfig.desktop.wayland.enable && lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.wprs;
      defaultText = literalExpression "config.myconfig.desktop.wayland.enable (if wprs is available on the platform)";
    };
    package = mkPackageOption pkgs "wprs" { };
    server = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = mdDoc ''
          Install the `wprsd` user service, so that this machine can act as
          the remote side of a wprs session.
        '';
      };
      autoStart = mkOption {
        type = types.bool;
        default = false;
        description = mdDoc ''
          Start `wprsd` automatically on login. If disabled, the unit is
          installed but has to be started manually
          (`systemctl --user start wprsd`).
        '';
      };
      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = literalExpression ''[ "--framerate=30" ]'';
        description = mdDoc ''
          Extra arguments passed to `wprsd`. See `wprsd --help`.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    systemd.user.services.wprsd = lib.mkIf cfg.server.enable {
      Unit = {
        Description = "wprsd, the server side of a wprs session";
        After = [ "network.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = lib.concatStringsSep " " (
          [ "${cfg.package}/bin/wprsd" ] ++ (map lib.escapeShellArg cfg.server.extraArgs)
        );
        Environment = [ "RUST_BACKTRACE=1" ];
        Restart = "on-failure";
      };
      Install = lib.mkIf cfg.server.autoStart {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
