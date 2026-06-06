# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Expose the age of the currently activated NixOS system as
# Prometheus metrics via the node_exporter textfile collector.
#
# Metrics produced:
#   * nixos_system_activation_timestamp_seconds
#       Unix timestamp when /run/current-system was created/switched
#       (i.e. when this generation became active on this host).
#   * nixos_system_age_seconds
#       Age of the current system in seconds at the time the textfile
#       was last refreshed (now - activation_timestamp).
#   * nixos_system_info{nixos_label,nixos_version,nixos_codename,
#                       configuration_revision}
#       Constant-1 info series carrying human-readable labels for the
#       running configuration. Use `nixos_system_age_seconds *
#       on(host) group_left(nixos_label) nixos_system_info` in
#       PromQL to join age + label.
#
# The textfile is refreshed by a systemd timer every minute; that is
# more than enough resolution for a metric whose unit is "days".
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  clientCfg = cfg.client;
  ageCfg = clientCfg.systemAge;

  # Information available at evaluation time. We deliberately do NOT
  # use builtins.currentTime here so the configuration stays pure and
  # `nix flake check` keeps working without `--impure`.
  nixosLabel = config.system.nixos.label or "unknown";
  nixosVersion = config.system.nixos.version or "unknown";
  nixosCodeName = config.system.nixos.codeName or "unknown";
  configurationRevision = config.system.configurationRevision or "";

  # Static info-metric content. Numeric metrics that depend on
  # /run/current-system mtime are computed by the refresh script at
  # runtime.
  staticInfoSnippet = ''
    # HELP nixos_system_info Static information about the running NixOS configuration.
    # TYPE nixos_system_info gauge
    nixos_system_info{nixos_label="${nixosLabel}",nixos_version="${nixosVersion}",nixos_codename="${nixosCodeName}",configuration_revision="${configurationRevision}"} 1
  '';

  # The shell script logic lives in scripts/myconfig-system-age-refresh.sh.
  # Site-specific values (textfileDir, staticInfoSnippet) are substituted
  # via pkgs.replaceVars (@ NAME @ substitution syntax).
  refreshScriptFile = pkgs.replaceVars ./scripts/myconfig-system-age-refresh.sh {
    textfileDir = ageCfg.textfileDir;
    staticInfoSnippet = staticInfoSnippet;
  };

  refreshScript = pkgs.writeShellApplication {
    name = "myconfig-system-age-refresh";
    runtimeInputs = with pkgs; [
      bash
      coreutils
    ];
    text = ''
      bash ${refreshScriptFile}
    '';
  };
in
{
  options.myconfig.observability.client.systemAge = with lib; {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Expose the age of the currently activated NixOS system as
        Prometheus metrics via the node_exporter textfile collector.
      '';
    };

    textfileDir = mkOption {
      type = types.str;
      default = "/var/lib/prometheus-node-exporter-text-files";
      description = ''
        Directory scraped by the node_exporter textfile collector.
        Must match `--collector.textfile.directory` passed to
        node_exporter (configured automatically by the
        myconfig.observability.client module).
      '';
    };

    refreshInterval = mkOption {
      type = types.str;
      default = "1min";
      description = ''
        How often the system-age textfile is refreshed. Uses systemd
        OnUnitActiveSec syntax.
      '';
    };
  };

  config = lib.mkIf (clientCfg.enable && ageCfg.enable) {
    # Make sure the textfile directory exists and is world-readable so
    # node_exporter (running as DynamicUser) can read it.
    systemd.tmpfiles.rules = [
      "d ${ageCfg.textfileDir} 0755 root root - -"
    ];

    # Oneshot service that (re-)writes the .prom file. Running it on
    # every activation guarantees the metric is up-to-date right after
    # nixos-rebuild switch, and the timer below keeps `*_age_seconds`
    # ticking without waiting for a reboot.
    systemd.services."myconfig-system-age" = {
      description = "Refresh NixOS system-age textfile metrics";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${refreshScript}/bin/myconfig-system-age-refresh";
      };
    };

    systemd.timers."myconfig-system-age" = {
      description = "Periodically refresh NixOS system-age textfile metrics";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "30s";
        OnUnitActiveSec = ageCfg.refreshInterval;
        Unit = "myconfig-system-age.service";
        AccuracySec = "10s";
      };
    };

    # Run the refresh once on every system activation so the metric
    # reflects the new generation immediately after `nixos-rebuild
    # switch`, without waiting for the timer to fire.
    system.activationScripts.myconfigSystemAgeRefresh = {
      text = ''
        if [ -d "${ageCfg.textfileDir}" ]; then
          ${refreshScript}/bin/myconfig-system-age-refresh || true
        fi
      '';
      deps = [ ];
    };
  };
}
