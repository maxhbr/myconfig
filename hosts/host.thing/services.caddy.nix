# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
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
  hostName = "${config.networking.hostName}.wg0.maxhbr.local";
  openWebuiPort =
    if config.myconfig.ai.container.open-webui.enable then
      config.myconfig.ai.container.open-webui.port
    else
      config.myconfig.ai.open-webui.port;
  litellmRouteConfig = lib.optionalString config.services.litellm.enable ''
    handle_path /litellm/* {
      reverse_proxy http://localhost:${toString config.services.litellm.port}
    }
  '';
  ollamaRouteConfig = lib.optionalString config.services.ollama.enable ''
    handle_path /ollama/* {
      reverse_proxy http://localhost:${toString config.services.ollama.port}
    }
  '';
  llamaSwapRouteConfig = lib.optionalString config.services.llama-swap.enable ''
    reverse_proxy http://localhost:${toString config.services.llama-swap.port}
  '';
  openWebuiRouteConfig =
    lib.optionalString
      (config.myconfig.ai.container.open-webui.enable || config.myconfig.ai.open-webui.enable)
      ''
        handle_path /open-webui/* {
          reverse_proxy http://localhost:${toString openWebuiPort}
        }
      '';
  comfyuiRouteConfig = lib.optionalString config.myconfig.ai.comfyui.enable ''
    handle_path /comfyui/* {
      reverse_proxy http://localhost:8188
    }
  '';
  searxngRouteConfig = lib.optionalString config.services.searx.enable ''
    handle_path /searx/uwsgi/* {
      reverse_proxy http://localhost${toString config.services.searx.uwsgiConfig.http}
    }
    handle_path /searx/* {
      reverse_proxy http://localhost:${toString config.services.searx.settings.server.port}
    }
  '';
  n8nRouteConfig = lib.optionalString (config.services.n8n.enable || config.myconfig.containers.n8n.enable) ''
    handle_path /n8n/* {
      reverse_proxy http://localhost:5678
    }
  '';
in
{
  config = {
    services.searx.settings.server.base_url = lib.mkForce "${hostName}/searx/";
    services.n8n.environment = {
      WEBHOOK_URL = "https://${hostName}/";
      N8N_PROXY_HOPS = 1;
    };
    containers.n8n.config.services.n8n.environment = {
      WEBHOOK_URL = "https://${hostName}/";
      N8N_PROXY_HOPS = 1;
    };

    services.caddy = {
      enable = true;
      virtualHosts."${hostName}" = {
        inherit hostName;
        listenAddresses = [ (myconfig.metadatalib.getWgIp "${config.networking.hostName}") ];
        serverAliases = [
          "${config.networking.hostName}.wg0"
          (myconfig.metadatalib.getWgIp "${config.networking.hostName}")
        ];
        extraConfig = ''
          ${litellmRouteConfig}
          ${ollamaRouteConfig}
          ${openWebuiRouteConfig}
          ${comfyuiRouteConfig}
          ${llamaSwapRouteConfig}
          ${searxngRouteConfig}
          ${n8nRouteConfig}
        '';
      };
    };

    networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals config.services.caddy.enable [
      443
    ];
  };
}
