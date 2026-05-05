{
  pkgs,
  config,
  lib,
  ...
}:
let
  basicAuthPassword = "unsafe:isheix9aitoo8pooThie";
  basicAuthPasswordFile = pkgs.writeText "victoriametrics-basic-auth-password" basicAuthPassword;
in

{
  imports = [
    (
      {
        config,
        lib,
        ...
      }:

      let
        metricsServer = "localhost";
      in
      {
        services.prometheus.exporters.node = {
          enable = true;
          port = 9100;

          # Important: only local vmagent can scrape this.
          listenAddress = "127.0.0.1";

          enabledCollectors = [
            "logind"
            "systemd"
          ];
        };

        services.vmagent = {
          enable = true;

          prometheusConfig = {
            global = {
              scrape_interval = "15s";

              external_labels = {
                host = config.networking.hostName;
              };
            };

            scrape_configs = [
              {
                job_name = "node";
                static_configs = [
                  {
                    targets = [ "127.0.0.1:9100" ];
                  }
                ];
              }

              {
                job_name = "vmagent";
                static_configs = [
                  {
                    targets = [ "127.0.0.1:8429" ];
                  }
                ];
              }
            ];
          };

          remoteWrite = {
            url = "http://${metricsServer}:8428/api/v1/write";

            # Depending on your nixpkgs vmagent module version, basic auth may either
            # be supported here or may need to go through extraArgs.
            # If this attrset fails evaluation, use extraArgs below instead.
            basicAuthUsername = "vmagent";
            basicAuthPasswordFile = toString basicAuthPasswordFile;
          };
        };
      }
    )
  ];
  config = {
    services.victoriametrics = {
      enable = true;

      # Bind to your LAN/VPN interface, or use "0.0.0.0:8428" if firewall-restricted.
      listenAddress = "0.0.0.0:8428";

      # Optional but recommended.
      retentionPeriod = "90d";

      # Strongly recommended if exposed beyond loopback.
      basicAuthUsername = "vmagent";
      basicAuthPasswordFile = toString basicAuthPasswordFile;
    };

    services.grafana = {
      enable = true;

      settings = {
        server = {
          domain = "localhost";
          http_addr = "127.0.0.1";
          http_port = 3000;
        };

        security = {
          admin_user = "admin";
          admin_password = lib.mkDefault "admin";
          secret_key = lib.mkDefault "SW2YcwTIb9zpOOhoPsMm"; # will be overwritten in private
        };

        "auth.anonymous" = {
          enabled = false;
        };
      };

      provision.datasources.settings.datasources = [
        {
          name = "VictoriaMetrics";
          type = "prometheus";
          url = "http://127.0.0.1:8428";
          access = "proxy";
          isDefault = true;

          # If you enable VictoriaMetrics basic auth, Grafana also needs it.
          basicAuth = true;
          basicAuthUser = "vmagent";
          secureJsonData = {
            basicAuthPassword = "$__file{${toString basicAuthPasswordFile}}";
          };
        }
      ];
    };

    networking.firewall.allowedTCPPorts = [
      8428 # VictoriaMetrics remote_write endpoint
      3000 # Only if you want Grafana reachable directly over LAN.
    ];

  };
}
