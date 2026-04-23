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

  pathBasedServices = [
    {
      name = "litellm";
      path = "/litellm/*";
      port = config.services.litellm.port;
      enable = config.services.litellm.enable;
    }
    {
      name = "ollama";
      path = "/ollama/*";
      port = config.services.ollama.port;
      enable = config.services.ollama.enable;
    }
    {
      name = "open-webui";
      path = "/open-webui/*";
      port = openWebuiPort;
      enable = config.myconfig.ai.container.open-webui.enable || config.myconfig.ai.open-webui.enable;
    }
    {
      name = "comfyui";
      path = "/comfyui/*";
      port = 8188;
      enable = config.myconfig.ai.comfyui.enable;
    }
    {
      name = "n8n";
      path = "/n8n/*";
      port = 5678;
      enable = config.services.n8n.enable || config.myconfig.containers.n8n.enable;
    }
  ];

  extraConfig = lib.concatStringsSep "\n" (
    lib.flatten (
      map (
        svc:
        lib.optionals svc.enable [
          ''
            handle_path ${svc.path} {
              reverse_proxy http://localhost:${toString svc.port}
            }
          ''
        ]
      ) pathBasedServices
    )
  );

  subdomainServices = [
    {
      name = "llama-swap";
      subdomain = "llama-swap";
      port = config.services.llama-swap.port;
      enable = config.services.llama-swap.enable;
    }
  ];

  subdomainVhosts = lib.foldl' (
    acc: svc:
    acc
    // lib.optionalAttrs svc.enable {
      "${svc.subdomain}.${hostName}" = {
        hostName = "${svc.subdomain}.${hostName}";
        listenAddresses = [ (myconfig.metadatalib.getWgIp "${config.networking.hostName}") ];
        serverAliases = [
          "${svc.subdomain}.${config.networking.hostName}.wg0"
        ];
        extraConfig = ''
          reverse_proxy http://localhost:${toString svc.port}
        '';
      };
    }
  ) { } subdomainServices;

  allVhosts = {
    "${hostName}" = {
      inherit hostName;
      listenAddresses = [ (myconfig.metadatalib.getWgIp "${config.networking.hostName}") ];
      serverAliases = [
        "${config.networking.hostName}.wg0"
        (myconfig.metadatalib.getWgIp "${config.networking.hostName}")
      ];
      inherit extraConfig;
    };
  }
  // subdomainVhosts;

  searxngVhost = lib.mkIf config.services.searx.enable {
    searxngExtraConfig = ''
      handle_path /searx/uwsgi/* {
        reverse_proxy http://localhost${toString config.services.searx.uwsgiConfig.http}
      }
      handle_path /searx/* {
        reverse_proxy http://localhost:${toString config.services.searx.settings.server.port}
      }
    '';
  };

  searxngConfig = lib.mkIf (
    config.services.searx.enable
    && builtins.hasAttr "searxngExtraConfig" (lib.foldl' lib.recursiveUpdate { } searxngVhost)
  ) (builtins.head (builtins.map (v: v.searxngExtraConfig) (lib.attrValues searxngVhost)));

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
      virtualHosts =
        allVhosts
        // lib.mkIf config.services.searx.enable {
          "${hostName}" = {
            inherit hostName;
            listenAddresses = [ (myconfig.metadatalib.getWgIp "${config.networking.hostName}") ];
            serverAliases = [
              "${config.networking.hostName}.wg0"
              (myconfig.metadatalib.getWgIp "${config.networking.hostName}")
            ];
            extraConfig = extraConfig + "\n" + searxngConfig;
          };
        };
    };

    networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals config.services.caddy.enable [
      443
    ];
  };
}
