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
  haCfg = config.myconfig.smart-home.home-assistant;
  haPort = config.services.home-assistant.config.http.server_port or 8123;
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
        editor is reachable on LAN without going through a reverse proxy.
      '';
    };

    withNpmAndGcc = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to give Node-RED access to NPM and GCC at runtime so
        additional nodes can be installed via the Palette Manager.
        Required to install `node-red-contrib-home-assistant-websocket`
        via the Palette Manager UI.
      '';
    };

    homeAssistantUrl = mkOption {
      type = types.str;
      default = "http://localhost:${toString haPort}";
      defaultText = lib.literalExpression ''"http://localhost:''${haPort}"'';
      description = ''
        Base URL that Node-RED uses to reach Home Assistant. Used only
        as a documentation hint — the actual URL is configured inside
        Node-RED's flow editor when adding the HA server node.

        The `node-red-contrib-home-assistant-websocket` package (installed
        via the Palette Manager) connects to this URL using a Long-Lived
        Access Token created in the HA UI under:
        Profile → Security → Long-Lived Access Tokens.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.node-red = {
      enable = true;
      port = cfg.port;
      withNpmAndGcc = cfg.withNpmAndGcc;
    };

    # Start Node-RED after Home Assistant so the HA WebSocket API is
    # available when Node-RED boots and the HA contrib nodes try to connect.
    systemd.services.node-red = lib.mkIf haCfg.enable {
      after = [ "home-assistant.service" ];
      wants = [ "home-assistant.service" ];
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.exposeOnLan [ cfg.port ];
  };
}
