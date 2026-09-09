# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# ccusage metrics exporter: runs `ccusage --json` (see
# ../myconfig.ai.dev/programs/programs.ccusage) against the monitored user's local
# coding-agent session files (Claude Code, Codex, OpenCode, pi, ...) and
# emits the daily token/cost breakdowns as Prometheus metrics via the
# node_exporter textfile collector.
#
# This module only makes sense when ccusage is actually part of the
# system, so its default follows `myconfig.ai.dev.ccusage.enable` (which in
# turn is auto-enabled wherever `myconfig.ai.dev.enable` is set):
#
#   * client.enable && ccusageExporter.enable(default: ai.ccusage.enable)
#
# The systemd unit runs as root with `ProtectHome=read-only` so it can
# read the monitored user's session directories while writing only into
# the root-owned textfile directory. ccusage needs no network (the
# nixpkgs package has the LiteLLM pricing table baked in), so there is
# no network-online dependency.
#
# Metrics produced (all carry `user="<monitoredUser>"`; see also
# scripts/myconfig-ccusage-refresh.sh for the full list):
#
#   * ccusage_scrape_success{user}
#   * ccusage_scrape_timestamp_seconds{user}
#   * ccusage_daily_total_tokens{user,period}      period = "YYYY-MM-DD"
#   * ccusage_daily_input_tokens{user,period}
#   * ccusage_daily_output_tokens{user,period}
#   * ccusage_daily_cache_creation_tokens{user,period}
#   * ccusage_daily_cache_read_tokens{user,period}
#   * ccusage_daily_cost_usd{user,period}
#   * ccusage_model_input_tokens{user,period,model} per-model breakdown
#   * ccusage_model_output_tokens{user,period,model}
#   * ccusage_model_cache_creation_tokens{user,period,model}
#   * ccusage_model_cache_read_tokens{user,period,model}
#   * ccusage_model_cost_usd{user,period,model}
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
  ccusageCfg = clientCfg.ccusageExporter;
  ageCfg = clientCfg.systemAge;

  # The shell script logic lives in scripts/myconfig-ccusage-refresh.sh.
  # Site-specific values are substituted via pkgs.replaceVars
  # (@ NAME @ substitution syntax).
  #
  # replaceVars forces `preferLocalBuild = true; allowSubstitutes = false`
  # in its derivation, which breaks distributed builds (see
  # client.system-age.nix for the full explanation). Override both back to
  # their stdenv defaults so the derivation can be built directly on the
  # remote builder.
  refreshScriptFile =
    (pkgs.replaceVars ./scripts/myconfig-ccusage-refresh.sh {
      textfileDir = ccusageCfg.textfileDir;
      monitoredUser = ccusageCfg.user;
      userHome = "/home/${ccusageCfg.user}";
      lookbackDays = toString ccusageCfg.lookbackDays;
      timeoutSeconds = toString ccusageCfg.timeoutSeconds;
    }).overrideAttrs
      {
        preferLocalBuild = false;
        allowSubstitutes = true;
      };

  refreshScript = pkgs.writeShellApplication {
    name = "myconfig-ccusage-refresh";
    runtimeInputs = with pkgs; [
      bash
      ccusage # the package from myconfig.ai.dev.ccusage, so both stay in sync
      coreutils # mktemp, chmod, date, timeout
      jq
    ];
    text = ''
      bash ${refreshScriptFile}
    '';
  };
in
{
  options.myconfig.observability.client.ccusageExporter = with lib; {
    enable = mkOption {
      type = types.bool;
      # Default follows the ccusage module: when the host ships ccusage
      # (auto-enabled by the myconfig.ai.dev umbrella), its usage metrics are
      # exported too. A host can still opt out explicitly.
      default = config.myconfig.ai.dev.ccusage.enable;
      defaultText = lib.literalExpression "config.myconfig.ai.dev.ccusage.enable";
      description = ''
        Expose coding-agent token/cost usage from `ccusage --json` as
        Prometheus metrics via the node_exporter textfile collector.
        Only meaningful when `myconfig.ai.dev.ccusage` is enabled (the
        default mirrors it).
      '';
    };

    user = mkOption {
      type = types.str;
      default = myconfig.user;
      defaultText = lib.literalExpression "myconfig.user";
      description = ''
        User whose coding-agent session directories are read
        (`~/.claude`, `~/.config/opencode`, ...). Also the value of the
        `user` label on every emitted sample.
      '';
    };

    textfileDir = mkOption {
      type = types.str;
      default = ageCfg.textfileDir;
      defaultText = lib.literalExpression "config.myconfig.observability.client.systemAge.textfileDir";
      description = ''
        Directory scraped by the node_exporter textfile collector.
        Defaults to the same directory used by `client.systemAge` so
        no extra `--collector.textfile.directory` flag is needed.
      '';
    };

    lookbackDays = mkOption {
      type = types.int;
      default = 30;
      description = ''
        How many days of history the exporter emits (`ccusage --since`).
        0 means all time. Older series drop out of the textfile on
        every refresh, keeping the series count bounded.
      '';
    };

    refreshInterval = mkOption {
      type = types.str;
      default = "10min";
      description = ''
        How often ccusage is re-run and the textfile is refreshed.
        Uses systemd OnUnitActiveSec syntax. Parsing a few weeks of
        session files is cheap, and the underlying data only changes
        while an agent session is actually running.
      '';
    };

    timeoutSeconds = mkOption {
      type = types.int;
      default = 120;
      description = ''
        Maximum runtime for a single ccusage invocation before the
        unit fails and the exporter emits scrape_success=0.
      '';
    };
  };

  config = lib.mkIf (clientCfg.enable && ccusageCfg.enable) {
    # The textfile directory is already created by client.system-age in
    # the default setup; provide it independently here anyway (same
    # pattern as client.weather-exporter.nix).
    systemd.tmpfiles.rules = [
      "d ${ccusageCfg.textfileDir} 0755 root root - -"
    ];

    systemd.services."myconfig-ccusage" = {
      description = "Refresh coding-agent usage metrics from ccusage";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${refreshScript}/bin/myconfig-ccusage-refresh";

        # Runs as root: needs read access to the monitored user's home
        # (session JSONL files) and write access to the /var/lib
        # textfile dir. Everything else is locked down like the
        # weather exporter.
        User = "root";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ ccusageCfg.textfileDir ];
        # read-only access to the user's home is the whole point of
        # running as root; the script must never write there.
        ProtectHome = "read-only";
        PrivateTmp = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        SystemCallArchitectures = "native";
      };
    };

    systemd.timers."myconfig-ccusage" = {
      description = "Periodically refresh coding-agent usage metrics";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "5min";
        OnUnitActiveSec = ccusageCfg.refreshInterval;
        Unit = "myconfig-ccusage.service";
        AccuracySec = "1min";
        Persistent = true;
      };
    };
  };
}
