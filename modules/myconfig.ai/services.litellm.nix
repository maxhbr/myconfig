# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  pkgs,
  lib,
  config,
  ...
}:
let
in

{

  imports = [
    {
      config = lib.mkIf (config.myconfig.ai.enable && config.services.litellm.enable) {
        services.postgresql = {
          enable = true;
          port = 5432;
          ensureDatabases = [ "mydatabase" ];
          authentication = pkgs.lib.mkOverride 10 ''
            #type database DBuser origin-address auth-method
            local all      all     trust
            # ipv4
            host  all      all     127.0.0.1/32   trust
            # ipv6
            host  all      all     ::1/128        trust
          '';
          initialScript = pkgs.writeText "backend-initScript" ''
            CREATE ROLE litellm WITH LOGIN PASSWORD 'litellm' CREATEDB;
            CREATE DATABASE litellm;
            GRANT ALL PRIVILEGES ON DATABASE litellm TO litellm;
          '';
        };
      };
    }
  ];
  config = lib.mkIf (config.myconfig.ai.enable && config.services.litellm.enable) {
    services.litellm = {
      host = "127.0.0.1";
      port = 4000;
      # settings.general_settings = {
      #   store_prompts_in_spend_logs = true;
      #   disable_spend_logs = false;
      #   maximum_spend_logs_retention_period = "30d";
      #   database_url = "postgresql://litellm:litellm@127.0.0.1:${toString config.services.postgresql.port}/litellm";
      # };
      settings.model_list = lib.optionals config.services.ollama.enable (
        map (model: {
          model_name = "ollama/${model}";
          litellm_params = {
            model = "ollama/${model}";
            api_base = "http://${config.services.ollama.host}:${toString config.services.ollama.port}";
          };
        }) config.services.ollama.loadModels
      );
    };

    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          (writeShellApplication {
            name = "litellm-logs";
            text = ''
              set -euo pipefail
              journalctl --follow --pager-end --unit litellm.service
            '';
          })
          (writeShellApplication {
            name = "litellm-restart";
            text = ''
              set -euo pipefail
              echo "Restarting LiteLLM..."
              sudo systemctl restart litellm.service
              echo "LiteLLM restarted. Check status with: systemctl status litellm.service"
            '';
          })
        ];
      }
    ];
  };
}
