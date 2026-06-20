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

  # Build the list of priority levels to collect: 0 (emergency) through
  # journalMinPriority (inclusive).  When journalMinPriority is null every
  # level is collected via a single block with no PRIORITY match.
  priorityLevels =
    if alloyCfg.journalMinPriority == null then
      [ null ] # single block, no PRIORITY filter
    else
      lib.lists.range 0 alloyCfg.journalMinPriority;

  # Build the `matches` string for one source block.
  # Combines an optional PRIORITY filter with an optional single-unit filter.
  # (The `matches` attribute uses systemd journal match syntax: AND-only,
  # space-separated FIELD=VALUE pairs.  Range filters and != are not
  # supported by Alloy; exclude-unit filtering is handled via relabel rules.)
  makeMatches =
    prio:
    let
      prioExpr = if prio == null then "" else "PRIORITY=${toString prio}";
      unitExpr =
        if alloyCfg.journalUnits != [ ] then
          # Only the first listed unit can be expressed as an exact match here;
          # additional units require separate source blocks (not supported yet).
          "_SYSTEMD_UNIT=${lib.head alloyCfg.journalUnits}"
        else
          "";
      parts = lib.filter (s: s != "") [
        prioExpr
        unitExpr
      ];
    in
    lib.concatStringsSep " " parts;

  # Render one loki.source.journal block.
  # `label` is the River component label (string), `prio` is an int or null.
  makeJournalBlock =
    label: prio:
    let
      matchesStr = makeMatches prio;
      # Rendered as a complete line (with trailing newline) or empty string.
      matchesLine = lib.optionalString (matchesStr != "") "  matches       = \"${matchesStr}\"\n";
    in
    # Use plain string concatenation so `matchesLine` indentation is literal,
    # not subject to Nix heredoc stripping.
    "loki.source.journal \"${label}\" {\n"
    + "  max_age       = \"${alloyCfg.journalMaxAge}\"\n"
    + matchesLine
    + "  relabel_rules = loki.relabel.journal.rules\n"
    + "  forward_to    = [loki.write.default.receiver]\n"
    + "  labels        = {\n"
    + "    job  = \"systemd-journal\",\n"
    + "    host = \"${hostName}\",\n"
    + "  }\n"
    + "}\n";

  # One block per priority level (or a single block when priority is null).
  journalBlocks = lib.concatMapStrings (
    prio:
    let
      label = if prio == null then "system" else "system_p${toString prio}";
    in
    makeJournalBlock label prio
  ) priorityLevels;

  # Exclude-unit relabel rules: drop any entry whose unit label matches an
  # excluded unit.  (The old `match = [...]` with `!=` syntax is not
  # supported by the Alloy `matches` attribute.)
  excludeRelabelRules = lib.concatMapStrings (u: ''
    rule {
      source_labels = ["unit"]
      regex         = "^${lib.escapeRegex u}$"
      action        = "drop"
    }
  '') alloyCfg.excludeUnits;

  # Alloy uses its own River-flavoured configuration language. We render
  # it from Nix into a plain text file.
  # excludeRelabelRules lines already include leading 2-space indent.
  excludeRelabelBlock = lib.optionalString (alloyCfg.excludeUnits != [ ]) (
    "\n" + excludeRelabelRules
  );

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
    ${excludeRelabelBlock}}

    ${journalBlocks}'';
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
        If non-empty, restrict collection to the first listed systemd unit
        via a `matches = "_SYSTEMD_UNIT=<unit>"` filter on each generated
        source block.  Only the first entry is used; filtering by multiple
        units simultaneously is not supported by Alloy's `matches` attribute
        (which is AND-only).  Leave empty to collect from all units.
      '';
    };

    excludeUnits = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        List of systemd units to exclude from collection.  Implemented as
        relabel `drop` rules on the `unit` label (after the unit name has
        been extracted from `__journal__systemd_unit`).  Useful for
        filtering out chatty services without maintaining an exhaustive
        allowlist.
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
