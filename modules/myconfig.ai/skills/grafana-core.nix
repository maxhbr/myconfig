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
  # `rev`+`hash` pin lives in the nvfetcher-generated `_sources/generated.nix`
  # (bumped by `nix run nixpkgs#nvfetcher` / a scheduled CI job), not in
  # flake.lock. See ../../../nvfetcher.toml.
  grafana-skills = (pkgs.callPackage ../../../_sources/generated.nix { }).grafana-skills.src;
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
    # Register the skill sources; `skills/default.nix` applies them to every
    # enabled agent harness via the `handcrafted` registry.
    myconfig.ai.skills.handcrafted = skillsAttrs;
  };
}
