# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.myconfig.ai.open-webui;
in
{
  options.myconfig.ai.open-webui = with lib; {
    enable = mkEnableOption "myconfig.ai.services.open-webui";

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Host to bind Open WebUI to";
    };

    port = mkOption {
      type = types.int;
      default = 8080;
      description = "Port for Open WebUI";
    };
  };

  config = lib.mkIf (config.myconfig.ai.enable && cfg.enable) {
    services.open-webui = {
      enable = true;
      host = cfg.host;
      port = cfg.port;

      environment = {
        WEBUI_AUTH = "False";
        ANONYMIZED_TELEMETRY = "False";
        DO_NOT_TRACK = "True";
        SCARF_NO_ANALYTICS = "True";
        OLLAMA_BASE_URL = "http://${config.services.ollama.host}:${toString config.services.ollama.port}";
        OLLAMA_API_BASE_URL = "http://${config.services.ollama.host}:${toString config.services.ollama.port}/api";
      };
    };

    # Ensure ollama is enabled and running
    services.ollama.enable = lib.mkDefault true;

    # Add home-manager shell scripts for managing Open WebUI
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          (writeShellApplication {
            name = "open-webui-logs";
            text = ''
              set -euo pipefail
              journalctl --follow --pager-end --unit open-webui.service
            '';
          })
          (writeShellApplication {
            name = "open-webui-restart";
            text = ''
              set -euo pipefail
              echo "Restarting Open WebUI..."
              sudo systemctl restart open-webui.service
              echo "Open WebUI restarted. Check status with: systemctl status open-webui.service"
            '';
          })
        ];
      }
    ];
  };
}
