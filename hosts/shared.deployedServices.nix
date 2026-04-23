# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:
{
  options.myconfig.deployedServices =
    with lib;
    mkOption {
      type = types.attrsOf (
        types.listOf (
          types.submodule {
            options = {
              name = mkOption {
                type = types.str;
                description = "Service name, used as subdomain";
              };
              port = mkOption {
                type = types.int;
                description = "Port on which the service listens";
              };
              ip = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "Optional custom IP address for the service";
              };
            };
          }
        )
      );
      default = { };
      description = "Map of hostnames to their deployed services";
    };

  config = {
    myconfig.deployedServices = {
      thing = [
        {
          name = "n8n";
          port = 5678;
        }
        {
          name = "llama-swap";
          port = 33656;
        }
        {
          name = "litellm";
          port = 4000;
        }
        {
          name = "ollama";
          port = 11434;
        }
        {
          name = "open-webui";
          port = 8080;
        }
        {
          name = "comfyui";
          port = 8188;
        }
        {
          name = "searxng";
          port = 8080;
        }
      ];
    };
  };
}
