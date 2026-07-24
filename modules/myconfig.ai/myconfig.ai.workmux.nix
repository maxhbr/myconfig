# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.workmux — workmux, "parallel development in tmux with git
# worktrees" (https://github.com/raine/workmux). It is a terminal-native
# companion to agentic coding harnesses, so it is enabled by default whenever
# the AI tooling, the dev profile, and tmux are all active on the host. The
# package is consumed directly from the upstream flake input
# (inputs.workmux.packages.${system}.default).
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.myconfig.ai.workmux;
  workmuxPkg = inputs.workmux.packages.${pkgs.system}.default;
in
{
  options.myconfig.ai.workmux = {
    enable = lib.mkEnableOption "workmux, parallel development in tmux with git worktrees";
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      {
        home.packages = [ workmuxPkg ];
      }
    ];
  };
}
