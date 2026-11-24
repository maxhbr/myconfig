# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:

let
  osconfig = config;
  callLib = file: import file { inherit lib pkgs; };
  mkWorkmuxWorktree = callLib ../../fns/workmux-worktree.nix;

  copilotWorktree = mkWorkmuxWorktree {
    name = "github-copilot-cli-worktree";
    agentName = "copilot";
    # workmux's built-in `copilot` agent behaviour (prompt injection /
    # resume flags) — the binary is `copilot`, unlike the package name.
    agentType = "copilot";
    innerPkg = pkgs.github-copilot-cli;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
in
{
  options.myconfig = with lib; {
    ai.dev.github-copilot-cli = {
      enable = mkEnableOption "myconfig.ai.dev.github-copilot-cli";
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.github-copilot-cli.enable {
    myconfig.ai.dev.workmux.agents.copilot = copilotWorktree.agent;

    # mysbx tier integration (../../../../mysbx). The podman backends get
    # the binary via their own `agentPackagesByFlag` default
    # (../../../../mysbx/podman.nix); the other backends need it on the
    # sandbox PATH. Unlike claude-code/codex nothing of the host
    # config is mounted: `~/.copilot` mixes credentials (OAuth tokens
    # of `copilot login`) with settings and sessions in ONE root,
    # like hermes (`../../../../modules/myconfig.ai.dev/hermes-agent/mysbx.nix`) — a sandboxed session
    # starts from the CLI's own first-run/login flow instead, and
    # `.copilot` persists per repository as a state-dir
    # (../../../../mysbx/docs/design/config.md D15).
    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ pkgs.github-copilot-cli ];
      agents.copilot.command = "copilot";
      config.stateDirs = [ ".copilot" ];
    };

    home-manager.sharedModules = [
      {
        myconfig.persistence.directories = [
          ".config/.copilot"
          ".local/state/.copilot"
        ];
        home.packages = [ copilotWorktree.wrapper ];
      }
    ];
  };
}
