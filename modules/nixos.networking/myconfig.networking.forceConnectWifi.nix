# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generic `force-connect-wifi` helper plus per-SSID wrappers
# (`force-connect-<ssid>`) generated from
# `myconfig.networking.forceConnectWifi.ssids`.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.networking.forceConnectWifi;

  force-connect-wifi = pkgs.writeShellApplication {
    name = "force-connect-wifi";
    runtimeInputs = with pkgs; [
      networkmanager
      gnugrep
      coreutils
    ];
    text = builtins.readFile ./force-connect-wifi.sh;
  };

  # Package names must be free of characters that are awkward in store paths.
  sanitizeName = ssid: lib.replaceStrings [ " " "/" ] [ "_" "_" ] ssid;

  mkWrapper =
    ssid:
    pkgs.writeShellApplication {
      name = "force-connect-${sanitizeName ssid}";
      runtimeInputs = [ force-connect-wifi ];
      text = ''
        exec force-connect-wifi ${lib.escapeShellArg ssid} "$@"
      '';
    };
in
{
  options.myconfig.networking.forceConnectWifi = {
    enable = lib.mkEnableOption "myconfig.networking.forceConnectWifi" // {
      default = cfg.ssids != [ ];
      defaultText = lib.literalExpression "ssids != [ ]";
    };
    ssids = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "MyNetwork" ];
      description = ''
        SSIDs for which a `force-connect-<ssid>` command is created.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      { home.packages = [ force-connect-wifi ] ++ map mkWrapper cfg.ssids; }
    ];
  };
}
