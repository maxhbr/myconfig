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
  aiCfg = config.myconfig.ai;
  workmuxPkg = inputs.workmux.packages.${pkgs.system}.default;
  # The workmux flake input also provides its source tree, from which the
  # per-agent status-tracking extensions/plugins are deployed verbatim (the
  # same files `workmux setup` would copy).
  workmuxSrc = inputs.workmux;

  yamlFormat = pkgs.formats.yaml { };

  # --- declarative equivalent of `workmux setup` -------------------------
  # `workmux setup` detects installed agents and drops status-tracking hooks
  # into each agent's config so panes report 🤖/💬/✅ in tmux window names.
  # We wire the exact same artefacts declaratively instead, gated on which
  # agent this host actually enables. See the embedded resources in the
  # workmux source: `.pi/extensions/`, `resources/opencode/`,
  # `.codex/hooks/workmux-status.json`, `.claude-plugin/plugin.json`.
  statusCmd = status: "workmux set-window-status ${status}";
  # A hook "group" with no matcher, as used by codex and (mostly) claude.
  hookGroup = status: {
    hooks = [
      {
        type = "command";
        command = statusCmd status;
      }
    ];
  };
  # Claude Code hooks (from `.claude-plugin/plugin.json`), merged into
  # `programs.claude-code.settings.hooks`.
  claudeStatusHooks = {
    UserPromptSubmit = [ (hookGroup "working") ];
    Notification = [
      {
        matcher = "permission_prompt|elicitation_dialog";
        hooks = [
          {
            type = "command";
            command = statusCmd "waiting";
          }
        ];
      }
    ];
    PostToolUse = [ (hookGroup "working") ];
    Stop = [ (hookGroup "done") ];
  };
  # Codex hooks (from `.codex/hooks/workmux-status.json`), written to
  # `~/.codex/hooks.json` via `programs.codex.hooks`.
  codexStatusHooks = {
    UserPromptSubmit = [ (hookGroup "working") ];
    PermissionRequest = [ (hookGroup "waiting") ];
    PostToolUse = [ (hookGroup "working") ];
    SubagentStart = [ (hookGroup "working") ];
    SubagentStop = [ (hookGroup "done") ];
    Stop = [ (hookGroup "done") ];
  };

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
  # parallel worktree agents: it opens a session with the sidebar + dashboard
  # and attaches/switches to it. The agent status-tracking hooks that the
  # dashboard/sidebar rely on are installed declaratively (see `config`
  # below), so there is no runtime `workmux setup` step here.
  tmux-workmux = pkgs.writeShellApplication {
    name = "tmux-workmux";
    runtimeInputs = [
      cfg.package
      pkgs.tmux
      pkgs.coreutils
    ];
    text = ''
      session=workmux

      # Create the session detached if it does not exist yet.
      if ! tmux has-session -t "=$session" 2>/dev/null; then
        tmux new-session -d -s "$session"
      fi

      # Bootstrap the sidebar + dashboard once, tracked via a session option so
      # a half-created session (e.g. from an older, buggy version) still gets
      # its dashboard on the next run instead of a bare shell.
      #
      # `send-keys` takes a *pane* target and tmux only strips the `=`
      # exact-match prefix for session/window targets, so `-t "=$session"`
      # fails inside tmux with `can't find pane: =workmux`. The trailing colon
      # (`=$session:`) forces session resolution and uses its active pane.
      if [ "$(tmux show-options -t "=$session:" -qv @workmux_bootstrapped)" != 1 ]; then
        tmux set-option -t "=$session:" @workmux_bootstrapped 1
        tmux send-keys -t "=$session:" 'workmux sidebar --session; workmux dashboard' Enter
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
  imports = [ ./jail.nix ];

  options.myconfig.ai.workmux = {
    enable = lib.mkEnableOption "workmux, parallel development in tmux with git worktrees";

    statusTracking.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Declaratively install workmux agent status-tracking hooks (the
        declarative equivalent of `workmux setup`) for every coding agent
        enabled on this host, so panes report agent status in tmux window
        names / the dashboard / the sidebar. Disable to manage the hooks
        yourself.
      '';
    };

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
        # Nerdfonts are available in this terminal setup, so pin the icon
        # setting declaratively. Without an explicit `nerdfont` key workmux
        # runs an interactive "Nerdfont Setup" prompt on first use (when no
        # PUA glyphs are present in the config), which blocks non-interactive
        # worktree creation. `true` enables the nicer nerdfont glyphs.
        nerdfont = true;
      }
      # Default `workmux add` (no explicit `--agent`) to the `pi` named
      # agent, but only on hosts that actually enable pi — otherwise the
      # `agents:` map has no `pi` entry and workmux would fall back to
      # running the bare `pi` command. workmux resolves the top-level
      # `agent` key through the `agents` map (see `resolve_selected_agent`).
      // lib.optionalAttrs aiCfg.pi-coding-agent.enable { agent = "pi"; }
      // {
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
    ]
    # Declarative `workmux setup`: install the status-tracking artefacts for
    # each agent this host enables. Each agent uses whichever mechanism its
    # home-manager module exposes (native hook/settings options where a
    # merge is required, a plain file drop otherwise).
    ++ lib.optional cfg.statusTracking.enable {
      config = lib.mkMerge [
        (lib.mkIf aiCfg.pi-coding-agent.enable {
          home.file.".pi/agent/extensions/workmux-status.ts".source =
            "${workmuxSrc}/.pi/extensions/workmux-status.ts";
        })
        (lib.mkIf aiCfg.opencode.enable {
          home.file.".config/opencode/package.json".source = "${workmuxSrc}/resources/opencode/package.json";
          home.file.".config/opencode/plugins/workmux-status.ts".source =
            "${workmuxSrc}/resources/opencode/plugins/workmux-status.ts";
        })
        (lib.mkIf aiCfg.codex.enable {
          programs.codex.hooks = codexStatusHooks;
          programs.codex.settings.features.hooks = true;
        })
        (lib.mkIf aiCfg.claude-code.enable {
          programs.claude-code.settings.hooks = claudeStatusHooks;
        })
      ];
    };
  };
}
