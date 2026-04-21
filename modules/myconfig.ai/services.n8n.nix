# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  config = lib.mkIf config.services.n8n.enable {
    services.n8n = {
      environment = {
        GENERIC_TIMEZONE = "Europe/Amsterdam";
        N8N_PORT = 5678;
        N8N_DIAGNOSTICS_ENABLED = false;
        N8N_VERSION_NOTIFICATIONS_ENABLED = false;
        N8N_ENCRYPTION_KEY_FILE = lib.mkDefault "/run/n8n/encryption-key";
        N8N_RUNNERS_AUTH_TOKEN_FILE = lib.mkDefault "/run/n8n/runner-token";
      };

      taskRunners = {
        enable = true;
        runners = {
          javascript.enable = true;
          python.enable = true;
        };
      };
    };

    # environment.persistence."/persistent/cache".directories = [
    #   config.services.n8n.environment.N8N_USER_FOLDER
    # ];

    systemd.tmpfiles.rules = [
      "d /run/n8n 0750 n8n n8n -"
    ];

    # services.traefik.dynamicConfigOptions.http = rec {
      # routers.to-n8n = {
      #   rule = "Host(`${publicFqdn}`) && (PathRegexp(`^/webhook(-test)?/.*`) || PathRegexp(`^/form/.*`))";
      #   service = "n8n";
      # };
      # services."${routers.to-n8n.service}".loadBalancer.servers = [
      #   {
      #     url = "http://127.0.0.1:${builtins.toString config.services.n8n.environment.N8N_PORT}";
      #   }
      # ];
    # };
  };
}
