# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.nono-agent-sandbox — run coding agents inside the nono
# capability-based sandbox (https://nono.sh/docs).
#
# This module generates `agent-nono-*` wrapper commands for the enabled
# agents (pi, opencode, claude-code, codex, etc.), analogous to the
# `agent-bubblewrap-*` wrappers but using nono's Landlock/seccomp-based
# isolation instead of bubblewrap.
#
# Entry points:
#
#   * `agent-nono-pi`           — pi-coding-agent in a nono sandbox
#   * `agent-nono-opencode`     — opencode in a nono sandbox
#   * `agent-nono-claude`       — claude-code in a nono sandbox
#   * `agent-nono-codex`        — codex in a nono sandbox
#   * ... (for each enabled agent)
#
# Like the bubblewrap wrappers, these refuse to run from $HOME to avoid
# exposing your entire home directory. Run from a project subdirectory.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  osconfig = config;
  callLib = file: import file { inherit lib pkgs; };
  callNonoLib =
    file:
    import file {
      inherit
        lib
        pkgs
        osconfig
        ;
    };
  nono-app = callNonoLib ./fns/nono-app.nix;

  # Make the `workmux` binary available inside the sandboxes (for the
  # `workmux set-window-status` status hooks and `workmux merge`/`remove` from
  # a worktree pane) whenever workmux is enabled.
  workmuxDevTools = lib.optional osconfig.myconfig.ai.workmux.enable osconfig.myconfig.ai.workmux.package;

  # Helper to build a nono-sandboxed agent wrapper with sensible defaults.
  mkNonoAgent =
    {
      name,
      pkg,
      userDataDirs ? [ ],
      extraDevTools ? [ ],
    }:
    nono-app {
      inherit name pkg userDataDirs;
      extraDevTools = extraDevTools ++ workmuxDevTools;
      # Forward the shared myconfig.ai.nono.fwdEnvs plus standard vars.
      extraFwdEnv = [ ];
      # Refuse to run from $HOME (same as jail-app.nix).
      rejectHomeCwd = true;
    };

  # pi-coding-agent wrapper
  agent-nono-pi = mkNonoAgent {
    name = "agent-nono-pi";
    pkg = pkgs.nixos-unstable.pi-coding-agent;
    userDataDirs = [
      ".pi"
      ".agents"
    ];
  };

  # opencode wrapper
  agent-nono-opencode = mkNonoAgent {
    name = "agent-nono-opencode";
    pkg = pkgs.opencode;
    userDataDirs = [
      ".config/opencode"
      ".local/share/opencode"
      ".local/state/opencode"
      ".config/mcp"
    ];
  };

  # claude-code wrapper
  agent-nono-claude = mkNonoAgent {
    name = "agent-nono-claude";
    pkg = pkgs.claude-code;
    userDataDirs = [
      ".claude"
      ".config/claude-code"
    ];
  };

  # codex wrapper
  agent-nono-codex = mkNonoAgent {
    name = "agent-nono-codex";
    pkg = pkgs.codex;
    userDataDirs = [
      ".codex"
      ".config/codex"
    ];
  };

  # Collect all nono wrappers into a list.
  nonoWrappers = [
    agent-nono-pi
    agent-nono-opencode
    agent-nono-claude
    agent-nono-codex
  ];
in
{
  options.myconfig.ai.nono-agent-sandbox = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable `agent-nono-*` sandbox wrappers for coding agents.
        When enabled, generates `agent-nono-pi`, `agent-nono-opencode`,
        `agent-nono-claude`, `agent-nono-codex` (for each enabled agent)
        that run the agent inside the nono capability-based sandbox.

        These are analogous to the `agent-bubblewrap-*` wrappers but use
        nono's Landlock/seccomp isolation. Like the bubblewrap wrappers,
        they refuse to run from $HOME to avoid exposing your entire home
        directory.
      '';
    };
  };

  config = lib.mkIf config.myconfig.ai.nono-agent-sandbox.enable {
    home-manager.sharedModules = [
      {
        home.packages = nonoWrappers;
      }
    ];
  };
}
