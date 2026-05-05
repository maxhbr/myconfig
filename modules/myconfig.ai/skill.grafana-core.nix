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
    rev = "ccbb23df0073b3fbbbbc4c95cae4b00cfbfb6823";
    hash = "sha256-5D8OSeNAKoyE6YO3v7GliQVhHzYgt4qopWwWJS8QpjI=";
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
