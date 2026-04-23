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
        in
        lib.listToAttrs (
          lib.map (
            {
              name,
              port,
              ip,
            }:
            (lib.nameValuePair "${name}.${baseHostName}" {
              hostName = "${name}.${baseHostName}";
              listenAddresses = [ (myconfig.metadatalib.getWgIp "${config.networking.hostName}") ];
              serverAliases = [
                "${toString port}.${baseHostName}"
                "${name}.${config.networking.hostName}.wg0"
                "${toString port}.${config.networking.hostName}.wg0"
              ];
              extraConfig = ''
                reverse_proxy http://${ip}:${toString port}
              '';
            })
          ) config.myconfig.deployedServices.services."${config.networking.hostName}"
        );
    };
    networking.extraHosts =
      let
        genExtraHost =
          host:
          { name, port, ... }:
          let
            wgIp = myconfig.metadatalib.getWgIp host;
          in
          "${wgIp} ${name}.${host}.wg0.maxhbr.local\n${wgIp} ${toString port}.${host}.wg0.maxhbr.local";
      in
      lib.concatStringsSep "\n" (
        lib.attrValues (
          lib.foldl' (
            acc: host:
            acc
            // lib.listToAttrs (
              lib.map (service: {
                name = "${service.name}.${host}";
                value = genExtraHost host service;
              }) config.myconfig.deployedServices.services.${host}
            )
          ) { } (builtins.attrNames config.myconfig.deployedServices.services)
        )
      );
  };
}
