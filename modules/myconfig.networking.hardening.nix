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
#   * `shipAttackLogs` adds dedicated Alloy journal sources for sshd and
#     the kernel. The generic journal source filters at
#     `journalMinPriority` (default: warning), which would drop both the
#     sshd auth failures and the firewall's `refused connection:` lines —
#     they are logged at info. Loki recording rules on the observability
#     host (host.loki.nix) turn those two streams into Prometheus series.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.networking.hardening;

  # node_exporter runs as a DynamicUser and reads this directory through
  # --collector.textfile.directory (wired by myconfig.observability.client
  # from the systemAge textfileDir).
  textfileDir = config.myconfig.observability.client.systemAge.textfileDir;

  clientEnabled = config.myconfig.observability.client.enable;
  alloyEnabled = clientEnabled && config.myconfig.observability.client.alloy.enable;

  exportMetrics = cfg.exportMetrics && clientEnabled;
  shipAttackLogs = cfg.shipAttackLogs && alloyEnabled;

  fail2banPackage = config.services.fail2ban.package;
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
        Log packets the firewall refuses. Without this a port scan or a
        brute-force run leaves no trace on the host, which makes the
        ban rule above the only evidence that something happened. This
        is also the log stream the attacks dashboard parses for source
        IPs and probed ports.
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
        Ship sshd and kernel journal streams to Loki in addition to the
        priority-filtered generic stream. Required for the per-source-IP
        and probed-port breakdowns: `Failed password`/`Failed publickey`
        and `refused connection:` are logged at info, which the generic
        source drops at the default `journalMinPriority`.
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
    # The directory is normally created by myconfig.observability.client's
    # systemAge sub-module; this rule only matters when that is off.
    systemd.tmpfiles.rules =
      lib.mkIf (exportMetrics && !config.myconfig.observability.client.systemAge.enable)
        [
          "d ${textfileDir} 0755 root root - -"
        ];

    systemd.services.myconfig-fail2ban-metrics = lib.mkIf (exportMetrics && cfg.banSshAuthFailures) {
      description = "Publish fail2ban jail counters as node_exporter textfile metrics";
      after = [ "fail2ban.service" ];
      wants = [ "fail2ban.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "myconfig-fail2ban-metrics" ''
          set -eu
          # fail2ban may still be starting; skip rather than publish a
          # half-written or stale file.
          if ! ${fail2banPackage}/bin/fail2ban-client status >/dev/null 2>&1; then
            exit 0
          fi
          tmp="$(mktemp "${textfileDir}/.fail2ban.XXXXXX")"
          trap 'rm -f "$tmp"' EXIT
          bash ${pkgs.writeText "fail2ban-textfile.sh" (builtins.readFile ./myconfig.observability/scripts/fail2ban-textfile.sh)} "$tmp"
          # node_exporter is a DynamicUser and reads this directory
          # unprivileged, so the sample must be world-readable (mktemp
          # creates it 0600).
          chmod 0644 "$tmp"
          mv -f "$tmp" "${textfileDir}/fail2ban.prom"
        '';
      };
    };

    systemd.timers.myconfig-fail2ban-metrics = lib.mkIf (exportMetrics && cfg.banSshAuthFailures) {
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
        mkSource = label: matches: ''
          loki.source.journal "${label}" {
            max_age       = "${config.myconfig.observability.client.alloy.journalMaxAge}"
            matches       = "${matches}"
            relabel_rules = loki.relabel.journal.rules
            forward_to    = [loki.write.default.receiver]
            labels        = {
              job          = "systemd-journal",
              log_stream   = "${label}",
              host         = "${hostName}",
            }
          }
        '';
      in
      lib.concatStringsSep "\n" [
        (mkSource "sshd" "_SYSTEMD_UNIT=sshd.service")
        (mkSource "kernel" "_TRANSPORT=kernel")
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
