# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — Workmux agent registrations (plan §29).
#
# Register the microvm-claude / microvm-pi / microvm-codex / microvm-opencode
# agents into the EXISTING `myconfig.ai.workmux.agents` registry (the same
# registry the pi / claude-code / codex / opencode modules use). We do NOT
# modify the workmux module or invent a parallel mechanism — an agent is just
# a `{ type; command; }` entry that workmux renders into
# `~/.config/workmux/config.yaml` and launches inside the freshly created
# worktree pane.
#
# The registered agents keep Workmux as the FRONTEND (§46: do not replace it):
# workmux still owns the worktree, the tmux pane, task naming, status hooks
# and cleanup (`workmux merge` / `workmux remove`). The phase-3 host launcher
# `agent-microvm` is only the BACKEND: each agent's `command` is a thin
# launcher that, from inside the workmux worktree pane, resolves the linked
# main repository (the standalone-clone source), derives the microVM task name
# from the workmux branch, and execs
#
#     sudo agent-microvm run --attach \
#         --name <task> --repository <main-repo> --agent <bin>
#
# passing the repo path, the workmux task name and the agent binary as
# SEPARATE argv (never a shell string) so the launcher's strict task/agent
# validation applies. No network-relaxation flags are passed, so the guest
# runs under the SECURE proxy-only profile (the module default; §31).
#
# The user workflow therefore stays exactly:
#
#     workmux add --agent microvm-claude feature-name
#
# Everything is gated on BOTH `cfg.enable` and `config.myconfig.ai.workmux.enable`,
# so a disabled feature — or a host without workmux — produces zero side
# effects. NOTE: this BOTH-gate is deliberately STRICTER than the existing
# agents (e.g. programs.claude-code registers gated only on its own
# `claude-code.enable`, not on workmux). The extra `workmux.enable` guard is
# required for this phase: the microvm agents are meaningless without workmux
# as the frontend.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  workmuxCfg = config.myconfig.ai.workmux;

  # Fixed agent set (§29): the workmux `agents.<name>` key -> the guest agent
  # binary passed to `agent-microvm run --agent <bin>` (also the workmux
  # built-in `type` used for prompt injection / resume flags). The bin names
  # match the launcher's `validate_agent_name` set and the guest exe names.
  agentSpecs = {
    microvm-claude = {
      bin = "claude";
      type = "claude";
    };
    microvm-pi = {
      bin = "pi";
      type = "pi";
    };
    microvm-codex = {
      bin = "codex";
      type = "codex";
    };
    microvm-opencode = {
      bin = "opencode";
      type = "opencode";
    };
  };

  # The command workmux launches inside the worktree pane for a given agent.
  # It resolves the main repo from the worktree (the same git-common-dir ->
  # dirname resolution the shared fns/workmux-worktree.nix launcher uses), maps
  # the workmux branch to a launcher-safe task name, and hands everything to
  # `agent-microvm run` as separate argv. `agent-microvm` is a host system
  # package (installed by launcher.nix when the feature is enabled) resolved
  # from PATH; `sudo` is the system wrapper (the launcher requires root for
  # mounts + systemctl, §20).
  #
  # NOTE (implicit deps / follow-up): `sudo` and `agent-microvm` are resolved
  # from the inherited/ambient PATH rather than `runtimeInputs` — post-`sudo`,
  # `agent-microvm` resolves via sudoers `secure_path` ->
  # /run/current-system/sw/bin (where launcher.nix installs it). This phase
  # adds NO passwordless-sudoers rule for `agent-microvm`, so the pane will
  # prompt interactively for a password on first launch. That is acceptable
  # for an interactive tmux workflow; a dedicated sudoers rule is left to a
  # later phase.
  mkLauncher =
    agentName: spec:
    pkgs.writeShellApplication {
      name = "${agentName}-microvm-launch";
      runtimeInputs = with pkgs; [
        git
        coreutils
        gnused
      ];
      text = ''
        # Runs inside a workmux-created worktree pane. Resolve the linked main
        # repository (the standalone-clone SOURCE for the microVM) from the
        # worktree's shared git dir; the launcher itself re-validates it.
        git_common_dir="$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null || true)"
        if [ -z "$git_common_dir" ]; then
          echo "${agentName}-microvm-launch: not inside a git repository" >&2
          exit 1
        fi
        main_repo="$(dirname "$git_common_dir")"

        # Map the workmux branch to a microVM task name the launcher accepts
        # (strict [a-zA-Z0-9._-], no leading '-', no '..', <=64 chars).
        # Disallowed bytes (e.g. '/') collapse to '-'; we truncate to 64 to
        # stay under the launcher's cap (a long branch would otherwise make
        # the backend die); the launcher re-validates the result.
        branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
        task="$(printf '%s' "$branch" \
          | tr -c 'a-zA-Z0-9._-' '-' \
          | sed -e 's#\.\.*#.#g' \
                -e 's#--*#-#g' \
                -e 's#^[-.]*##' \
                -e 's#[-.]*$##' \
          | cut -c1-64 \
          | sed -e 's#[-.]*$##')"
        if [ -z "$task" ]; then
          echo "${agentName}-microvm-launch: could not derive a task name from branch '$branch'" >&2
          exit 1
        fi

        # Backend only: workmux stays the frontend (worktree/pane/status/
        # cleanup). --attach runs 'agent-run ${spec.bin}' in the guest for
        # this pane and tears the VM down on exit (the workspace clone is
        # kept). No network-relaxation flags -> secure proxy-only profile.
        exec sudo agent-microvm run --attach \
          --name "$task" \
          --repository "$main_repo" \
          --agent ${lib.escapeShellArg spec.bin}
      '';
    };

  mkAgent = agentName: spec: {
    type = spec.type;
    command = lib.getExe (mkLauncher agentName spec);
  };
in
{
  # Register only when the feature is enabled AND workmux is active on this
  # host (the existing agents guard their registration the same way).
  config = lib.mkIf (cfg.enable && workmuxCfg.enable) {
    myconfig.ai.workmux.agents = lib.mapAttrs mkAgent agentSpecs;
  };
}
