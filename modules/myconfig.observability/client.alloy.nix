# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana Alloy on each client, forwards systemd journal and selected
# log files to the central Loki instance running on the observability
# host (see ./host.loki.nix).
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.observability;
  clientCfg = cfg.client;
  alloyCfg = clientCfg.alloy;
  wgIp = myconfig.metadatalib.getWgIp cfg.host_hostname;

  hostName = config.networking.hostName;
  lokiUrl = "http://${wgIp}:${toString cfg.lokiPort}/loki/api/v1/push";

  # River helper: render an optional list as a River array literal.
  # '["a", "b"]' or '[]' if empty.
  renderRiverList = items: builtins.toJSON items;

  # Build match expressions for excluded units.
  # Each becomes '_SYSTEMD_UNIT!=<unit>' in the River match list.
  excludeMatchExprs = map (u: "_SYSTEMD_UNIT!=${u}") alloyCfg.excludeUnits;

  # Alloy uses its own River-flavoured configuration language. We render
  # it from Nix into a plain text file.
  alloyConfig = ''
        // Managed by myconfig.observability.client.alloy
        // Forwards systemd journal entries to Loki at ${lokiUrl}

        loki.write "default" {
          endpoint {
            url = "${lokiUrl}"
          }
          external_labels = {
            host = "${hostName}",
          }
        }

        loki.relabel "journal" {
          forward_to = []

          rule {
            source_labels = ["__journal__systemd_unit"]
            target_label  = "unit"
          }
          rule {
            source_labels = ["__journal__hostname"]
            target_label  = "nodename"
          }
          rule {
            source_labels = ["__journal_priority_keyword"]
            target_label  = "level"
          }
          rule {
            source_labels = ["__journal__transport"]
            target_label  = "transport"
          }
        }

        loki.source.journal "system" {
          max_age       = "${alloyCfg.journalMaxAge}"
    ${lib.optionalString (alloyCfg.journalMinPriority != null) ''
      priority      = ${toString alloyCfg.journalMinPriority}
    ''}
    ${lib.optionalString (alloyCfg.journalUnits != [ ]) ''
      units         = ${renderRiverList alloyCfg.journalUnits}
    ''}
    ${lib.optionalString (alloyCfg.excludeUnits != [ ]) ''
      match         = ${renderRiverList excludeMatchExprs}
    ''}
          relabel_rules = loki.relabel.journal.rules
          forward_to    = [loki.write.default.receiver]
          labels        = {
            job  = "systemd-journal",
            host = "${hostName}",
          }
        }
  '';
in
{
  options.myconfig.observability.client.alloy = with lib; {
    enable = mkOption {
      type = types.bool;
      default = clientCfg.enable;
      defaultText = literalExpression "config.myconfig.observability.client.enable";
      description = ''
        Whether to run Grafana Alloy on this client to forward logs
        (systemd journal) to the central Loki instance. Defaults to the
        value of `myconfig.observability.client.enable`.
      '';
    };

    journalMaxAge = mkOption {
      type = types.str;
      default = "1h";
      description = ''
        Maximum age of journal entries to ingest at startup.
        Keep this small on laptops to avoid CPU/thermal spikes
        on cold starts.
      '';
    };

    journalMinPriority = mkOption {
      type = types.nullOr types.int;
      default = 4; # warning
      description = ''
        Minimum syslog priority level to collect. Set to `null` to
        collect all levels. Syslog priority scale:

        0 = emergency
        1 = alert
        2 = critical
        3 = error
        4 = warning  (default)
        5 = notice
        6 = info
        7 = debug
      '';
    };

    journalUnits = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        If non-empty, only collect journal entries from the listed
        systemd units. Leave empty to collect from all units.
      '';
    };

    excludeUnits = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        List of systemd units to exclude from collection.
        Applied as `_SYSTEMD_UNIT!=<unit>` match expressions.
        Useful for filtering out chatty services without
        maintaining an exhaustive whitelist.
      '';
    };

    extraConfig = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Additional Alloy (River) configuration appended to the generated
        config. Component names must be unique.
      '';
    };
  };

  config = lib.mkIf (clientCfg.enable && alloyCfg.enable) {
    services.alloy.enable = true;

    # Install the rendered configuration into /etc/alloy/. Alloy reads
    # all *.alloy files in its config directory and reload-triggers on
    # changes (see upstream module).
    environment.etc."alloy/myconfig.alloy".text =
      alloyConfig + lib.optionalString (alloyCfg.extraConfig != "") ("\n" + alloyCfg.extraConfig);
  };
}
