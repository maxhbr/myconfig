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
              redirect = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = ''
                  If set, Caddy will issue an HTTP redirect to this URL
                  instead of acting as a reverse proxy. Useful when the
                  service should be reached directly on its upstream
                  host (e.g. to avoid websocket / cookie issues through
                  the proxy).
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
    center = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        The central host. For example "vserver".
        If set it generats proxy configuration for all services on the other hosts.
        If another host ("nur") provides "hass.nuc.wg0.maxhbr.local" it generates "hass.nuc.vserver.wg0.maxhbr.local" and redirects to it.
        It redirects to the https port but ignores the certificate (which is self generated).
      '';
    };
  };

  config = lib.mkIf (config.myconfig.deployedServices.services != { }) {
    # When this host serves Caddy for deployedServices, open the
    # corresponding HTTP(S) ports on the WireGuard interface so peers
    # can actually reach it. Each Caddy vhost binds explicitly to the
    # host's `wg0` IP, so opening these ports on `wg0` only is
    # sufficient and keeps the public interface untouched.
    networking.firewall.interfaces."wg0".allowedTCPPorts =
      lib.mkIf config.myconfig.deployedServices.configureCaddy
        [
          80
          443
        ];
    services.caddy = lib.mkIf config.myconfig.deployedServices.configureCaddy {
      enable = lib.mkDefault true;
      virtualHosts =
        let
          hostName = config.networking.hostName;
          baseHostName = "${hostName}.wg0.maxhbr.local";
          allServices = config.myconfig.deployedServices.services;
          servicesList = allServices."${hostName}" or [ ];
          center = config.myconfig.deployedServices.center;
          isCenter = center != null && center == hostName;
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
                redirect,
              }:
              if port == null && redirect == null then
                [ ]
              else
                let
                  portAliasUnique = port != null && (portToService.${toString port} == name);
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
                  proxyConfig =
                    if redirect != null then
                      ''
                        ${noCacheHeaders}
                        redir ${redirect} permanent
                      ''
                    else
                      ''
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
          # When this host is the configured `center`, generate proxy
          # virtualHosts that re-expose services from every other host
          # under `${name}.${otherHost}.${centerBase}` and reverse-proxy
          # to the upstream Caddy at
          # `https://${name}.${otherHost}.wg0.maxhbr.local`. The upstream
          # uses `tls internal` (self-signed), so TLS verification is
          # disabled for the proxy transport.
          centerProxyHosts =
            if !isCenter then
              { }
            else
              let
                centerIp = myconfig.metadatalib.getWgIp hostName;
                otherHosts = lib.filter (h: h != hostName) (lib.attrNames allServices);
                proxyForService =
                  otherHost:
                  {
                    name,
                    port,
                    forceHttps,
                    redirect,
                    ...
                  }:
                  if port == null && redirect == null then
                    [ ]
                  else
                    let
                      upstreamFqdn = "${name}.${otherHost}.wg0.maxhbr.local";
                      proxiedFqdn = "${name}.${otherHost}.${baseHostName}";
                      shortAlias = "${name}.${otherHost}.${hostName}.wg0";
                      # The upstream Caddy serves the service on HTTPS
                      # with a self-signed `tls internal` cert, so we
                      # always proxy via HTTPS for the https variant and
                      # disable verification.
                      httpsProxy = ''
                        reverse_proxy https://${upstreamFqdn} {
                          header_up Host {upstream_hostport}
                          transport http {
                            tls
                            tls_insecure_skip_verify
                          }
                        }
                      '';
                      # When the upstream is `forceHttps = false`, it
                      # additionally serves the service on plain HTTP;
                      # proxy via http there to avoid unnecessary TLS.
                      # `header_up Host` is required so the upstream
                      # Caddy matches its own `http://${upstreamFqdn}`
                      # site rather than seeing the center-proxied
                      # Host and falling through to its auto-https
                      # redirect (which would otherwise 308 every
                      # request back to https://*.vserver.*).
                      httpProxy = ''
                        reverse_proxy http://${upstreamFqdn} {
                          header_up Host {upstream_hostport}
                        }
                      '';
                    in
                    if forceHttps then
                      [
                        (lib.nameValuePair proxiedFqdn {
                          hostName = proxiedFqdn;
                          listenAddresses = [ centerIp ];
                          serverAliases = [ shortAlias ];
                          extraConfig = ''
                            tls internal
                            ${httpsProxy}
                          '';
                        })
                      ]
                    else
                      [
                        (lib.nameValuePair "https://${proxiedFqdn}" {
                          hostName = "https://${proxiedFqdn}";
                          listenAddresses = [ centerIp ];
                          serverAliases = [ "https://${shortAlias}" ];
                          extraConfig = ''
                            tls internal
                            ${httpsProxy}
                          '';
                        })
                        (lib.nameValuePair "http://${proxiedFqdn}" {
                          hostName = "http://${proxiedFqdn}";
                          listenAddresses = [ centerIp ];
                          serverAliases = [ "http://${shortAlias}" ];
                          extraConfig = httpProxy;
                        })
                      ];
              in
              lib.listToAttrs (
                lib.concatMap (
                  otherHost: lib.concatMap (proxyForService otherHost) allServices.${otherHost}
                ) otherHosts
              );
        in
        serviceHosts // indexHost // centerProxyHosts;
    };
    networking.extraHosts =
      let
        baseDomain = "wg0.maxhbr.local";
        center = config.myconfig.deployedServices.center;
        allServices = config.myconfig.deployedServices.services;
        allHosts = lib.attrNames allServices;
        centerWgIp = if center != null then myconfig.metadatalib.getWgIp center else null;
        # When a center is configured, also publish
        # `${name}.${otherHost}.${center}.${baseDomain}` mappings
        # pointing at the center's WG IP for every service on every
        # non-center host.
        # Suffix appended to short `${name}.${otherHost}` / `${port}.${otherHost}`
        # labels to form the center-proxied FQDN. Mirrors the vhost names
        # generated above when this host is the center.
        centerSuffix = lib.optionalString (center != null) "${center}.${baseDomain}";
        centerAliasLines = lib.optionals (center != null) (
          lib.concatMap (
            hostname:
            lib.concatMap (
              { name, port, ... }:
              [
                "${centerWgIp} ${name}.${hostname}.${centerSuffix}"
              ]
              ++ lib.optionals (port != null) [
                "${centerWgIp} ${toString port}.${hostname}.${centerSuffix}"
              ]
            ) allServices.${hostname}
          ) (lib.filter (h: h != center) allHosts)
        );
      in
      lib.concatStringsSep "\n" (
        (lib.concatMap (
          hostname:
          let
            wgIp = myconfig.metadatalib.getWgIp hostname;
            baseHost = "${hostname}.${baseDomain}";
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
          ) allServices.${hostname})
        ) allHosts)
        ++ centerAliasLines
      );
  };
}
