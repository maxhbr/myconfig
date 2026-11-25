# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Network-facing hardening shared by hosts that are reachable from an
# untrusted network. Opt-in, because the parts that make sense on an
# internet-exposed server (brute-force banning, verbose auth logging,
# dropped-packet logging) only add noise on a desktop behind NAT.
#
# Feeding the "Attacks and reachability" dashboard
# (modules/myconfig.observability/dashboards/security-attacks.json):
#
#   * `exportMetrics` publishes fail2ban's jail counters through the
#     node_exporter textfile collector. fail2ban has no Prometheus
#     endpoint of its own.
#   * `shipAttackLogs` forwards SSH failures and refused TCP connections
#     that the generic warning-level journal source would miss. Loki
#     records per-host totals; Grafana queries the logs for IPs and ports.
{
  config,
  lib,
  pkgs,
  utils,
  ...
}:
let
  cfg = config.myconfig.networking.hardening;

  systemAge = config.myconfig.observability.client.systemAge;
  inherit (systemAge) textfileDir;

  clientEnabled = config.myconfig.observability.client.enable;
  alloyEnabled = clientEnabled && config.myconfig.observability.client.alloy.enable;

  exportMetrics = cfg.exportMetrics && clientEnabled && config.services.fail2ban.enable;
  shipAttackLogs = cfg.shipAttackLogs && alloyEnabled;
in
{
  options.myconfig.networking.hardening = {
    enable = lib.mkEnableOption "network hardening for externally reachable hosts";

    banSshAuthFailures = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Enable fail2ban for sshd. NixOS ships an `sshd` jail, and the
        module raises `services.openssh.settings.LogLevel` to `VERBOSE`
        on its own so failed attempts are observable.
      '';
    };

    logRefusedConnections = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Log refused TCP connection attempts. The attacks dashboard parses
        these kernel messages for source IPs and probed ports.
      '';
    };

    exportMetrics = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Publish fail2ban jail counters as Prometheus metrics through the
        node_exporter textfile collector.
      '';
    };

    fail2banMetricsInterval = lib.mkOption {
      type = lib.types.str;
      default = "30s";
      description = ''
        How often the fail2ban textfile is refreshed. Uses systemd
        `OnUnitActiveSec` syntax.
      '';
    };

    shipAttackLogs = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Ship only failed SSH password/public-key authentications and
        firewall-refused TCP connection attempts in the dedicated Loki
        streams. The generic journal priority filter remains unchanged.
      '';
    };

    provisionDashboard = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Provision the "Attacks and reachability" Grafana dashboard. Only
        has an effect on the host running Grafana
        (`myconfig.observability.host.enable`).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.logRefusedConnections = lib.mkDefault cfg.logRefusedConnections;
    services.fail2ban.enable = lib.mkDefault cfg.banSshAuthFailures;

    # ─── fail2ban metrics ────────────────────────────────────────────────
    # systemAge normally configures the shared directory and its collector.
    systemd.tmpfiles.rules = lib.mkIf (exportMetrics && !systemAge.enable) [
      "d ${textfileDir} 0755 root root - -"
    ];
    services.prometheus.exporters.node.extraFlags = lib.mkIf (exportMetrics && !systemAge.enable) [
      "--collector.textfile.directory=${textfileDir}"
    ];

    systemd.services.myconfig-fail2ban-metrics = lib.mkIf exportMetrics {
      description = "Publish fail2ban jail counters as node_exporter textfile metrics";
      after = [ "fail2ban.service" ];
      wants = [ "fail2ban.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = utils.escapeSystemdExecArgs [
          pkgs.runtimeShell
          ./myconfig.observability/scripts/fail2ban-textfile.sh
          "${config.services.fail2ban.package}/bin/fail2ban-client"
          textfileDir
        ];
      };
    };

    systemd.timers.myconfig-fail2ban-metrics = lib.mkIf exportMetrics {
      description = "Periodically refresh fail2ban textfile metrics";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "60s";
        OnUnitActiveSec = cfg.fail2banMetricsInterval;
        Unit = "myconfig-fail2ban-metrics.service";
        AccuracySec = "10s";
      };
    };

    # ─── attack log shipping ─────────────────────────────────────────────
    # Two extra journal sources, appended through the documented
    # `extraConfig` escape hatch of myconfig.observability.client.alloy.
    # They reuse the `journal` relabel rules and `default` write receiver
    # that the client module already defines, and carry a `log_stream`
    # label so the recording rules and dashboard can tell them apart from
    # the generic journal stream.
    myconfig.observability.client.alloy.extraConfig = lib.mkIf shipAttackLogs (
      let
        hostName = config.networking.hostName;
        mkSource = label: matches: messagePattern: ''
          loki.source.journal "${label}" {
            max_age       = "${config.myconfig.observability.client.alloy.journalMaxAge}"
            matches       = "${matches}"
            relabel_rules = loki.relabel.journal.rules
            forward_to    = [loki.process.${label}.receiver]
            labels        = {
              job          = "systemd-journal",
              log_stream   = "${label}",
              host         = "${hostName}",
            }
          }

          loki.process "${label}" {
            stage.match {
              selector = `{log_stream="${label}"} !~ "${messagePattern}"`
              action   = "drop"
            }
            forward_to = [loki.write.default.receiver]
          }
        '';
      in
      lib.concatStringsSep "\n" [
        (mkSource "sshd" "_SYSTEMD_UNIT=sshd.service" "^Failed (password|publickey) for ")
        (mkSource "kernel" "_TRANSPORT=kernel" "^refused connection: ")
      ]
    );

    # ─── dashboard ───────────────────────────────────────────────────────
    services.grafana.provision.dashboards.settings =
      lib.mkIf (cfg.provisionDashboard && config.myconfig.observability.host.enable)
        {
          apiVersion = lib.mkDefault 1;
          providers = [
            {
              name = "myconfig-security-attacks";
              type = "file";
              disableDeletion = true;
              updateIntervalSeconds = 60;
              folder = "Security";
              options.path = pkgs.runCommand "security-attacks-dashboards" { } ''
                mkdir -p $out
                cp ${./myconfig.observability/dashboards/security-attacks.json} $out/security-attacks.json
              '';
            }
          ];
        };
  };
}
