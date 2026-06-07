# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.skills.grafana-core;
  grafana-skills = pkgs.fetchFromGitHub {
    owner = "grafana";
    repo = "skills";
    rev = "bc09fe7272b13a6ca32ca12ae3b8f3dff542af54";
    hash = "sha256-Y1ZpLl8IvPE4r/fOPsRPUWm7bdYLj8vle2ss7Assj0M=";
  };
  pluginName = "grafana-core";
  skillNames = [
    "alerting-irm"
    "alloy"
    "beyla"
    "dashboarding"
    "grafana-oss"
    "opentelemetry"
    "promql"
  ];
  skillsAttrs = lib.listToAttrs (
    map (name: {
      name = "${pluginName}-${name}";
      value = "${grafana-skills}/skills/${pluginName}/${name}";
    }) skillNames
  );
in
{
  options.myconfig.ai.skills.grafana-core = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.grafana-core";
  };
  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      {
        programs.opencode.skills = skillsAttrs;
        programs.claude-code.skills = skillsAttrs;
        programs.codex.skills = skillsAttrs;
      }
    ];
  };
}
