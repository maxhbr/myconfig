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
      default = 8888;
      description = "Port for Open WebUI, 8080 is used by litellm";
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
        ENABLE_FORWARD_USER_INFO_HEADERS = "True";
      }
      // lib.optionalAttrs config.services.litellm.enable (
        let
          # `host` may be a wildcard (e.g. "0.0.0.0") for external exposure;
          # rewrite to localhost for in-host clients.
          litellmHost =
            if config.services.litellm.host == "0.0.0.0" then "localhost" else config.services.litellm.host;
          base = "http://${litellmHost}:${toString config.services.litellm.port}";
        in
        {
          LITELLM_BASE_URL = base;
          LITELLM_API_BASE_URL = "${base}/v1";
        }
      );
    };

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
