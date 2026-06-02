# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.myconfig.smart-home.node-red;
in
{
  options.myconfig.smart-home.node-red = with lib; {
    enable = mkEnableOption "Node-RED flow-based automation service";

    port = mkOption {
      type = types.port;
      default = 1880;
      description = ''
        TCP port the Node-RED editor/runtime listens on. Not exposed to
        the LAN by default; reach it via a reverse proxy or SSH tunnel.
      '';
    };

    exposeOnLan = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to open `port` in the local firewall so the Node-RED
        editor is reachable on the LAN without going through a reverse proxy.
      '';
    };

    withNpmAndGcc = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to give Node-RED access to NPM and GCC at runtime so
        additional nodes can be installed via the Palette Manager.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.node-red = {
      enable = true;
      port = cfg.port;
      withNpmAndGcc = cfg.withNpmAndGcc;
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.exposeOnLan [ cfg.port ];
  };
}
