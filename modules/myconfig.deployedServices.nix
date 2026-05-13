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
                type = types.nullOr types.int;
                default = null;
                description = "Port on which the service listens (null for non-HTTP services)";
              };
              ip = mkOption {
                type = types.str;
                default = "localhost";
                description = "Optional custom IP address for the service";
              };
              forceHttps = mkOption {
                type = types.bool;
                default = true;
                description = "Whether to force HTTPS (redirect HTTP to HTTPS) for this service";
              };
              disableCache = mkOption {
                type = types.bool;
                default = false;
                description = ''
                  Whether to instruct browsers (and any intermediary
                  caches) to never cache responses from this service.
                  Caddy will override the upstream `Cache-Control`,
                  `Pragma` and `Expires` response headers.
                '';
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

  config = lib.mkIf (config.myconfig.deployedServices.services != { }) {
    services.caddy = lib.mkIf config.myconfig.deployedServices.configureCaddy {
      enable = lib.mkDefault true;
      virtualHosts =
        let
          hostName = config.networking.hostName;
          baseHostName = "${hostName}.wg0.maxhbr.local";
          servicesList = config.myconfig.deployedServices.services."${hostName}";
          allServices = config.myconfig.deployedServices.services;
          portToService = lib.listToAttrs (
            lib.map (s: {
              name = toString s.port;
              value = s.name;
            }) (lib.filter (s: s.port != null) servicesList)
          );
          indexHtml =
            let
              renderHostSection =
                host:
                let
                  hostBase = "${host}.wg0.maxhbr.local";
                  wgIp = myconfig.metadatalib.getWgIp host;
                  items = lib.concatMapStringsSep "\n" (
                    { name, port, ... }:
                    ''<li><a href="https://${name}.${hostBase}/">${name}</a>${
                      lib.optionalString (
                        port != null
                      ) ''<a class="port" href="http://${wgIp}:${toString port}">(:${toString port})</a>''
                    }</li>''
                  ) allServices.${host};
                  isCurrent = host == hostName;
                in
                ''
                      <section class="host${lib.optionalString isCurrent " current"}">
                        <h2>${host}${lib.optionalString isCurrent " (this host)"}</h2>
                        <ul>
                  ${items}
                        </ul>
                      </section>
                '';
              sortedHosts =
                let
                  hosts = lib.attrNames allServices;
                  current = lib.filter (h: h == hostName) hosts;
                  others = lib.filter (h: h != hostName) hosts;
                in
                current ++ (lib.sort (a: b: a < b) others);
              sections = lib.concatMapStrings renderHostSection sortedHosts;
            in
            pkgs.writeText "index.html" ''
              <!DOCTYPE html>
              <html lang="en">
              <head>
                <meta charset="UTF-8">
                <title>Services on ${hostName}</title>
                <style>
                  body { font-family: system-ui, sans-serif; max-width: 720px; margin: 2em auto; padding: 0 1em; color: #222; }
                  h1 { border-bottom: 2px solid #444; padding-bottom: 0.3em; }
                  h2 { margin-top: 1.5em; color: #555; }
                  .current h2 { color: #1a6; }
                  ul { list-style: none; padding-left: 0; }
                  li { padding: 0.25em 0; }
                  a { color: #06c; text-decoration: none; }
                  a:hover { text-decoration: underline; }
                  .port { color: #888; font-size: 0.9em; }
                  footer { margin-top: 2em; color: #888; font-size: 0.85em; }
                </style>
              </head>
              <body>
                <h1>Deployed services</h1>
                <p>Index served from <code>${baseHostName}</code></p>
              ${sections}
                <footer>generated by myconfig.deployedServices</footer>
              </body>
              </html>
            '';
          indexRoot = pkgs.runCommand "deployedServices-index" { } ''
            mkdir -p $out
            cp ${indexHtml} $out/index.html
          '';
          serviceHosts = lib.listToAttrs (
            lib.concatMap (
              {
                name,
                port,
                ip,
                forceHttps,
                disableCache,
              }:
              if port == null then
                [ ]
              else
                let
                  portAliasUnique = (portToService.${toString port} == name);
                  aliases =
                    (lib.optional portAliasUnique "${toString port}.${baseHostName}")
                    ++ [
                      "${name}.${hostName}.wg0"
                    ]
                    ++ (lib.optional portAliasUnique "${toString port}.${hostName}.wg0");
                  noCacheHeaders = lib.optionalString disableCache ''
                    header {
                      Cache-Control "no-store, no-cache, must-revalidate, max-age=0"
                      Pragma "no-cache"
                      Expires "0"
                      -ETag
                      -Last-Modified
                    }
                  '';
                  proxyConfig = ''
                    ${noCacheHeaders}
                    reverse_proxy http://${ip}:${toString port}
                  '';
                in
                if forceHttps then
                  [
                    (lib.nameValuePair "${name}.${baseHostName}" {
                      hostName = "${name}.${baseHostName}";
                      listenAddresses = [ (myconfig.metadatalib.getWgIp hostName) ];
                      serverAliases = aliases;
                      extraConfig = ''
                        tls internal
                        ${proxyConfig}
                      '';
                    })
                  ]
                else
                  [
                    (lib.nameValuePair "https://${name}.${baseHostName}" {
                      hostName = "https://${name}.${baseHostName}";
                      listenAddresses = [ (myconfig.metadatalib.getWgIp hostName) ];
                      serverAliases = lib.map (a: "https://${a}") aliases;
                      extraConfig = ''
                        tls internal
                        ${proxyConfig}
                      '';
                    })
                    (lib.nameValuePair "http://${name}.${baseHostName}" {
                      hostName = "http://${name}.${baseHostName}";
                      listenAddresses = [ (myconfig.metadatalib.getWgIp hostName) ];
                      serverAliases = lib.map (a: "http://${a}") aliases;
                      extraConfig = proxyConfig;
                    })
                  ]
            ) servicesList
          );
          indexHost = {
            "${baseHostName}" = {
              hostName = baseHostName;
              listenAddresses = [ (myconfig.metadatalib.getWgIp hostName) ];
              serverAliases = [ "${hostName}.wg0" ];
              extraConfig = ''
                tls internal
                root * ${indexRoot}
                file_server
              '';
            };
          };
        in
        serviceHosts // indexHost;
    };
    networking.extraHosts =
      let
        baseDomain = "wg0.maxhbr.local";
      in
      lib.concatStringsSep "\n" (
        lib.concatMap (
          hostname:
          let
            wgIp = myconfig.metadatalib.getWgIp hostname;
            baseHost = "${hostname}.wg0.maxhbr.local";
          in
          [
            "${wgIp} ${baseHost}"
          ]
          ++ (lib.concatMap (
            { name, port, ... }:
            [
              "${wgIp} ${name}.${baseHost}"
            ]
            ++ lib.optionals (port != null) [
              "${wgIp} ${toString port}.${baseHost}"
            ]
          ) config.myconfig.deployedServices.services.${hostname})
        ) (lib.attrNames config.myconfig.deployedServices.services)
      );
  };
}
