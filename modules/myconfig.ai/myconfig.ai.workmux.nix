# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.workmux — workmux, "parallel development in tmux with git
# worktrees" (https://github.com/raine/workmux). It is a terminal-native
# companion to agentic coding harnesses, so it is enabled by default whenever
# the AI tooling, the dev profile, and tmux are all active on the host. The
# package is consumed directly from the upstream flake input
# (inputs.workmux.packages.${system}.default).
#
# In addition to installing the binary, this module owns the *global* workmux
# configuration (`~/.config/workmux/config.yaml`). Coding-agent modules (pi,
# claude-code, opencode, ...) register "named agents" here via
# `myconfig.ai.workmux.agents.<name>` — typically a jailed/sandboxed launcher
# produced by `fns/workmux-worktree.nix`. A thin `<agent>-worktree` wrapper
# then runs `workmux add --agent <name>`, and workmux launches the launcher in
# the worktree pane. `workmux merge` / `workmux remove` handle cleanup, so no
# bespoke resume/cleanup scripts are needed anymore.
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

  yamlFormat = pkgs.formats.yaml { };

  # Drop attrs whose value is null so optional named-agent fields
  # (args/env/...) do not emit empty YAML keys.
  pruneNulls = lib.filterAttrs (_: v: v != null);

  workmuxConfig = pruneNulls (
    {
      agents = lib.mapAttrs (_: pruneNulls) cfg.agents;
    }
    // cfg.settings
  );
in
{
  options.myconfig.ai.workmux = {
    enable = lib.mkEnableOption "workmux, parallel development in tmux with git worktrees";

    package = lib.mkOption {
      type = lib.types.package;
      default = workmuxPkg;
      defaultText = lib.literalExpression "inputs.workmux.packages.\${system}.default";
      description = "The workmux package to install and drive the worktree wrappers.";
    };

    agents = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            type = lib.mkOption {
              type = lib.types.str;
              description = ''
                workmux built-in agent behaviour used for prompt injection and
                resume/skip-permission flags (e.g. "pi", "claude", "codex",
                "opencode").
              '';
            };
            command = lib.mkOption {
              type = lib.types.str;
              description = "Executable (or command string) workmux launches for this agent.";
            };
            args = lib.mkOption {
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
              default = null;
              description = "Literal arguments appended after the command, before injected prompts.";
            };
            env = lib.mkOption {
              type = lib.types.nullOr (lib.types.attrsOf lib.types.str);
              default = null;
              description = "Environment variables set for the agent process.";
            };
          };
        }
      );
      default = { };
      description = ''
        workmux "named agents" rendered under `agents:` in the generated
        global config. Coding-agent modules register their jailed worktree
        launchers here; select one with `workmux add --agent <name>`.
      '';
    };

    settings = lib.mkOption {
      type = yamlFormat.type;
      default = {
        # Default pane layout for `workmux add`: a single focused pane running
        # the selected agent (via the `<agent>` placeholder). Without this,
        # workmux only auto-launches an agent for projects that have a
        # CLAUDE.md, opening a bare shell otherwise.
        panes = [
          {
            command = "<agent>";
            focus = true;
          }
        ];
      };
      description = ''
        Free-form extra settings merged into `~/.config/workmux/config.yaml`
        alongside the generated `agents`. See
        https://workmux.raine.dev/guide/configuration for available keys.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      {
        home.packages = [ cfg.package ];
        home.file.".config/workmux/config.yaml".source =
          yamlFormat.generate "workmux-config.yaml" workmuxConfig;
      }
    ];
  };
}
