# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Reusable helper that turns a jailed/sandboxed coding-agent wrapper into a
# thin workmux-driven worktree launcher.
#
# workmux (https://github.com/raine/workmux) owns the full worktree +
# tmux-window lifecycle: `workmux add <branch>` creates a git worktree under
# `<project>__worktrees/<handle>`, opens a tmux window, and runs the selected
# agent in a pane; `workmux merge` / `workmux remove` tear everything down.
#
# This helper produces two derivations plus the data needed to register a
# workmux "named agent":
#
#   * `launcher`  — the command workmux actually runs *inside* the freshly
#                   created worktree pane. It resolves the linked main
#                   repository's shared git directory (so the jail/sandbox can
#                   bind it and git works from the worktree), exports the env
#                   vars the inner wrapper expects, then `exec`s the inner
#                   jailed/sandboxed agent binary. workmux appends the prompt
#                   flags (`-p ...`) for the configured agent `type`.
#
#   * `wrapper`   — the thin user-facing command (e.g. `jailed-pi-worktree`).
#                   It refuses to run outside tmux (workmux requires a running
#                   tmux server) and otherwise just execs
#                   `workmux add --agent <agentName> "$@"`, forwarding every
#                   argument (branch name, `--prompt`, `--base`, ...) straight
#                   through to workmux.
#
#   * `agent`     — `{ type; command; }` to be merged into
#                   `myconfig.ai.workmux.agents.<agentName>`, which the workmux
#                   module renders into `~/.config/workmux/config.yaml`.
#
# The worktree's shared git directory is exposed to the inner wrapper via two
# environment variables whose names are configurable (`mainRepoEnv`,
# `gitDirEnv`) so the same helper drives both the jail.nix-based wrappers
# (`PI_WORKTREE_MAIN_REPO` / `PI_WORKTREE_GIT_DIR`) and the hand-rolled
# bubblewrap wrappers (`WORKTREE_MAIN_REPO` / `WORKTREE_GIT_DIR`).
{
  lib,
  pkgs,
}:

{
  # User-facing thin wrapper command name, e.g. "jailed-pi-worktree".
  name,

  # workmux named-agent key selected via `workmux add --agent <agentName>`.
  # Also the key under `myconfig.ai.workmux.agents`.
  agentName,

  # workmux built-in agent behaviour used for prompt injection / resume flags
  # (one of "pi", "claude", "codex", "opencode", ...).
  agentType,

  # The jailed/sandboxed wrapper package launched inside the worktree pane.
  innerPkg,

  # The workmux package (added to the wrapper's PATH).
  workmuxPkg,

  # Environment variable names through which the resolved main-repo path (bound
  # read-only) and shared git dir (bound read-write) are handed to `innerPkg`.
  mainRepoEnv ? "PI_WORKTREE_MAIN_REPO",
  gitDirEnv ? "PI_WORKTREE_GIT_DIR",
}:

let
  launcher = pkgs.writeShellApplication {
    name = "${agentName}-workmux-launch";
    runtimeInputs = with pkgs; [
      git
      coreutils
    ];
    text = ''
      # Runs inside a workmux-created worktree pane. Resolve the linked main
      # repository's shared git directory so the jailed/sandboxed agent can
      # bind it read-write; without this git fails with "not a git repository"
      # because the worktree's `.git` file points into the main repo's
      # `.git/worktrees/<name>/`.
      git_common_dir="$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null || true)"
      if [ -z "$git_common_dir" ]; then
        echo "${agentName}-workmux-launch: not inside a git repository" >&2
        exit 1
      fi
      main_repo="$(dirname "$git_common_dir")"
      export ${mainRepoEnv}="$main_repo"
      export ${gitDirEnv}="$git_common_dir"
      exec ${lib.getExe innerPkg} "$@"
    '';
  };

  wrapper = pkgs.writeShellApplication {
    inherit name;
    runtimeInputs = [ workmuxPkg ];
    text = ''
      # workmux drives git worktrees *and* tmux windows; it is useless (and
      # errors deep inside) without a tmux server. Fail fast with a clear
      # message instead.
      if [ -z "''${TMUX:-}" ]; then
        echo "${name}: must be run inside a tmux session (workmux requires tmux)." >&2
        echo "${name}: start tmux first, then re-run from inside it." >&2
        exit 1
      fi

      # Everything after the command name is forwarded verbatim to
      # `workmux add`: the branch name plus any workmux flags such as
      # -p/--prompt, --base, -A/--auto-name, -b/--background, ...
      exec workmux add --agent ${agentName} "$@"
    '';
  };
in
{
  inherit launcher wrapper;
  agent = {
    type = agentType;
    command = lib.getExe launcher;
  };
}
