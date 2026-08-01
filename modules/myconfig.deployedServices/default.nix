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
  imports = [
    ./internalCa
  ];

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
              upstreamScheme = mkOption {
                type = types.enum [
                  "http"
                  "https"
                ];
                default = "http";
                description = ''
                  Scheme Caddy uses to talk to the upstream service.
                  Set to "https" for services that only speak TLS on
                  their upstream port (e.g. a UniFi gateway's web UI
                  on 192.168.1.1:443).
                '';
              };
              upstreamSkipTlsVerify = mkOption {
                type = types.bool;
                default = false;
                description = ''
                  When `upstreamScheme = "https"` and the upstream
                  presents a self-signed or otherwise untrusted cert
                  (e.g. UniFi appliances), set this to skip TLS
                  verification on the proxy → upstream hop. Has no
                  effect when `upstreamScheme = "http"`.
                '';
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
              excludeFromMonitoring = mkOption {
                type = types.bool;
                default = false;
                description = ''
                  When true, this service is excluded from blackbox
                  uptime monitoring (no probe target is generated for
                  it in the vmagent scrape job).
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

          internalCa = config.myconfig.deployedServices.internalCa;
          useAcme = internalCa.enable && internalCa.useAcmeForCaddy;

          # TLS directive used in every site block that previously
          # said `tls internal`. When `useAcmeForCaddy` is on we point
          # Caddy at the internal step-ca ACME directory; the root cert
          # is already in the system trust store (see
          # internalCa/default.nix) so no explicit `trusted_roots` is
          # strictly needed, but we pass it anyway for robustness in
          # case Caddy's CA bundle resolution differs.
          caddyTlsConfig =
            if useAcme then
              ''
                tls {
                  issuer acme {
                    dir ${internalCa.acmeDirectoryUrl}
                    trusted_roots ${internalCa.rootCert}
                  }
                }
              ''
            else
              "tls internal";

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
                upstreamScheme,
                upstreamSkipTlsVerify,
                forceHttps,
                disableCache,
                redirect,
                excludeFromMonitoring,
              }:
              if port == null && redirect == null then
                [ ]
              else
                let
                  aliases = [
                    "${name}.${hostName}.wg0"
                  ];
                  noCacheHeaders = lib.optionalString disableCache ''
                    header {
                      Cache-Control "no-store, no-cache, must-revalidate, max-age=0"
                      Pragma "no-cache"
                      Expires "0"
                      -ETag
                      -Last-Modified
                    }
                  '';
                  # When proxying to an HTTPS upstream we rewrite the
                  # Host, Origin and Referer headers to the upstream's
                  # own hostport. UniFi gateways (and most appliances
                  # with hard-coded internal FQDNs / IPs) validate
                  # these on WebSocket upgrades for CSRF, so without
                  # the rewrite e.g. `wss://.../api/ws/webrtc/local`
                  # fails the handshake.
                  #
                  # When the upstream cert is self-signed we also need
                  # `tls_insecure_skip_verify` inside the transport
                  # block.
                  proxyBlockLines =
                    lib.optionals (upstreamScheme == "https") [
                      "header_up Host {upstream_hostport}"
                      "header_up Origin ${upstreamScheme}://{upstream_hostport}"
                      "header_up Referer ${upstreamScheme}://{upstream_hostport}{uri}"
                    ]
                    ++ lib.optionals (upstreamScheme == "https" && upstreamSkipTlsVerify) [
                      "transport http {"
                      "  tls"
                      "  tls_insecure_skip_verify"
                      "}"
                    ];
                  proxyBlock =
                    lib.optionalString (proxyBlockLines != [ ])
                      " {\n${lib.concatMapStringsSep "\n" (l: "  ${l}") proxyBlockLines}\n}";
                  proxyConfig =
                    if redirect != null then
                      ''
                        ${noCacheHeaders}
                        redir ${redirect} permanent
                      ''
                    else
                      ''
                        ${noCacheHeaders}
                        reverse_proxy ${upstreamScheme}://${ip}:${toString port}${proxyBlock}
                      '';
                in
                if forceHttps then
                  [
                    (lib.nameValuePair "${name}.${baseHostName}" {
                      hostName = "${name}.${baseHostName}";
                      listenAddresses = [ (myconfig.metadatalib.getWgIp hostName) ];
                      serverAliases = aliases;
                      extraConfig = ''
                        ${caddyTlsConfig}
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
                        ${caddyTlsConfig}
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
                ${caddyTlsConfig}
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
        allServices = config.myconfig.deployedServices.services;
        allHosts = lib.attrNames allServices;
      in
      lib.concatStringsSep "\n" (
        lib.concatMap (
          hostname:
          let
            wgIp = myconfig.metadatalib.getWgIp hostname;
            baseHost = "${hostname}.${baseDomain}";
          in
          [
            "${wgIp} ${baseHost}"
          ]
          ++ (map ({ name, ... }: "${wgIp} ${name}.${baseHost}") allServices.${hostname})
        ) allHosts
      );
  };
}
