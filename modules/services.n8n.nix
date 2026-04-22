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
let 
  default_N8N_ENCRYPTION_KEY_FILE = "/run/n8n/encryption-key";
  default_N8N_RUNNERS_AUTH_TOKEN_FILE = "/run/n8n/runner-token";
  n8nConfig = 
    { config, pkgs, lib, ... }:
    {
      config = lib.mkIf config.services.n8n.enable {
          services.n8n = {
            environment = {
              GENERIC_TIMEZONE = "Europe/Amsterdam";
              N8N_PORT = 5678;
              N8N_DIAGNOSTICS_ENABLED = false;
              N8N_VERSION_NOTIFICATIONS_ENABLED = false;
              N8N_ENCRYPTION_KEY_FILE = lib.mkDefault default_N8N_ENCRYPTION_KEY_FILE;
              N8N_RUNNERS_AUTH_TOKEN_FILE = lib.mkDefault default_N8N_RUNNERS_AUTH_TOKEN_FILE;
            };

            taskRunners = {
              enable = true;
              runners = {
                javascript.enable = true;
                python.enable = true;
              };
            };
          };

          systemd.tmpfiles.rules = [
            "d /run/n8n 0750 n8n n8n -"
          ];
    };
  };

in
{
  options.myconfig = with lib; {
    containers.n8n = {
      enable = mkEnableOption "myconfig.containers.n8n";
    };
  };
  imports = [ n8nConfig ];
  config = lib.mkIf config.myconfig.containers.n8n.enable {
    containers.n8n = {
      autoStart = true;
      bindMounts = {
        "${default_N8N_ENCRYPTION_KEY_FILE}" = {
          hostPath = default_N8N_ENCRYPTION_KEY_FILE;
          isReadOnly = true;
        };
        "${default_N8N_RUNNERS_AUTH_TOKEN_FILE}" = {
          hostPath = default_N8N_RUNNERS_AUTH_TOKEN_FILE;
          isReadOnly = true;
        };
      };

      config = {
        imports = [ n8nConfig ];
        services.n8n.enable = true;
        nixpkgs.config.allowUnfree = true;
      };
    };

    # environment.persistence."/persistent/cache".directories = [
    #   config.services.n8n.environment.N8N_USER_FOLDER
    # ];
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
