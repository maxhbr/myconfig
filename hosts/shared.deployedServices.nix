# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  myconfig,
  pkgs,
  lib,
  ...
}:
{
  options.myconfig.deployedServices = with lib; {
    services = mkOption {
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
                type = types.str;
                default = "localhost";
                description = "Optional custom IP address for the service";
              };
            };
          }
        )
      );
      default = { };
      description = "Map of hostnames to their deployed services";
    };
    configureCaddy = mkEnableOption "configure caddy for this machine";
  };

  config = {
    myconfig.deployedServices.services = {
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
          name = "llama-swap-2";
          port = 33657;
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
    services.caddy = lib.mkIf config.myconfig.deployedServices.configureCaddy {
      enable = lib.mkDefault true;
      virtualHosts =
        let
          baseHostName = "${config.networking.hostName}.wg0.maxhbr.local";
          servicesList = config.myconfig.deployedServices.services."${config.networking.hostName}";
          portToService = lib.listToAttrs (
            lib.map (s: {
              name = toString s.port;
              value = s.name;
            }) servicesList
          );
        in
        lib.listToAttrs (
          lib.map (
            {
              name,
              port,
              ip,
            }:
            let
              portAliasUnique = (portToService.${toString port} == name);
            in
            (lib.nameValuePair "${name}.${baseHostName}" {
              hostName = "${name}.${baseHostName}";
              listenAddresses = [ (myconfig.metadatalib.getWgIp "${config.networking.hostName}") ];
              serverAliases =
                (lib.optional portAliasUnique "${toString port}.${baseHostName}")
                ++ [
                  "${name}.${config.networking.hostName}.wg0"
                ]
                ++ (lib.optional portAliasUnique "${toString port}.${config.networking.hostName}.wg0");
              extraConfig = ''
                tls internal
                reverse_proxy http://${ip}:${toString port}
              '';
            })
          ) servicesList
        );
    };
    networking.extraHosts =
      let
        baseDomain = "wg0.maxhbr.local";
      in
      lib.concatStringsSep "\n" (
        lib.concatMap (
          hostname: lib.concatMap (
            { name, port, ... }:
            let
              wgIp = myconfig.metadatalib.getWgIp hostname;
              baseHost = "${hostname}.wg0.maxhbr.local";
            in
            [
              "${wgIp} ${name}.${baseHost}"
              "${wgIp} ${toString port}.${baseHost}"
            ]
          ) config.myconfig.deployedServices.services.${hostname}
        ) (lib.attrNames config.myconfig.deployedServices.services)
      );
  };
}
