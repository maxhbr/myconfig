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

  # `tmux-workmux` bootstraps a dedicated "workmux" tmux session for driving
  # parallel worktree agents: it runs the one-time `workmux setup`, opens a
  # session with the sidebar + dashboard, and attaches/switches to it.
  tmux-workmux = pkgs.writeShellApplication {
    name = "tmux-workmux";
    runtimeInputs = [
      cfg.package
      pkgs.tmux
      pkgs.coreutils
    ];
    text = ''
      session=workmux

      # Run the one-time `workmux setup` (installs the agent status-tracking
      # hooks the dashboard/sidebar rely on) the first time this script is
      # used. workmux has no "already set up" flag, so track it with a state
      # sentinel and let setup itself be idempotent/interactive.
      state_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/workmux"
      sentinel="$state_dir/tmux-workmux.setup-done"
      if [ ! -e "$sentinel" ]; then
        workmux setup || true
        mkdir -p "$state_dir"
        touch "$sentinel"
      fi

      # Create the session detached if it does not exist yet, remembering
      # whether we just created it so the dashboard/sidebar are only added on
      # first creation (not on every re-attach).
      newly_created=0
      if ! tmux has-session -t "=$session" 2>/dev/null; then
        tmux new-session -d -s "$session"
        newly_created=1
      fi

      # For a freshly created session, launch the persistent status sidebar and
      # then the TUI dashboard in its main pane. Both run inside the session's
      # own tmux context via send-keys, in one command sequence so the
      # dashboard lands in the original shell pane regardless of the pane the
      # sidebar activates.
      if [ "$newly_created" -eq 1 ]; then
        tmux send-keys -t "=$session" 'workmux sidebar --session; workmux dashboard' Enter
      fi

      # Switch when already inside tmux (attach cannot be nested), attach
      # otherwise.
      if [ -n "''${TMUX:-}" ]; then
        exec tmux switch-client -t "=$session"
      else
        exec tmux attach-session -t "=$session"
      fi
    '';
  };
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
        home.packages = [
          cfg.package
          tmux-workmux
        ];
        home.file.".config/workmux/config.yaml".source =
          yamlFormat.generate "workmux-config.yaml" workmuxConfig;
      }
    ];
  };
}
