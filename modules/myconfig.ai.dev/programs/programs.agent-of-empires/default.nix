# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.agent-of-empires — Agent of Empires (`aoe`), a tmux based
# terminal session manager for AI coding agents
# (https://github.com/agent-of-empires/agent-of-empires).
#
# The package is consumed directly from the upstream flake input; upstream
# exposes `packages.<system>.default` (plain `aoe`) and
# `packages.<system>.aoe-with-web` (same binary built with `--features web`,
# bundling the React dashboard). `web = true` switches between the two.
#
# Unlike the siblings that are auto-enabled by the `myconfig.ai` umbrella
# (rtk, workmux), this is an external tool with a heavy from-source Rust build
# and it overlaps with the workmux workflow already used here, so it stays
# explicit opt-in per host: `enable` defaults to false and nothing switches it
# on implicitly.
#
# `aoe` needs tmux at runtime (Docker only for its optional sandboxing), so
# enabling this module also enables the tmux program. Nothing is executed at
# build or activation time — `aoe` creates its own state lazily on first run.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.agent-of-empires;
  aoePkgs = inputs.agent-of-empires.packages.${pkgs.system};
in
{
  options.myconfig = with lib; {
    ai.dev.agent-of-empires = {
      enable = mkEnableOption "myconfig.ai.dev.agent-of-empires";

      web = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Use the `aoe-with-web` build (`--features web`, includes the
          bundled web dashboard / remote access frontend) instead of the
          plain `aoe` build.
        '';
      };

      package = mkOption {
        type = types.package;
        default = if cfg.web then aoePkgs.aoe-with-web else aoePkgs.default;
        defaultText = literalExpression "inputs.agent-of-empires.packages.\${pkgs.system}.default";
        description = "The agent-of-empires package to install.";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    programs.tmux.enable = true;
    home-manager.sharedModules = [
      {
        home.packages = [ cfg.package ];
      }
    ];
  };
}
