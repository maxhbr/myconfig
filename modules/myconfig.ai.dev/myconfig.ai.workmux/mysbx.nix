# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.workmux.mysbx — the `mysbx` counterpart of
# `myconfig.ai.workmux.jail` (bubblewrap, `agent-bubblewrap-workmux-tmux`)
# and `myconfig.ai.workmux.sandbox` (microVM, `agent-qemu-workmux-tmux`).
#
# Same model as those two — one sandbox owns the whole tmux/workmux
# session, and the agents workmux launches run *inside* it, un-jailed —
# but expressed as `mysbx` CONFIGURATION instead of a Nix call site: no
# new wrapper binary is added here. `cd <repo> && mysbx` is the entry
# point, and with this module the interactive payload of that sandbox is
# the workmux session (../mysbx/docs/design/config.md D16,
# ../mysbx/docs/design/cli.md D11).
#
# What differs from the jail tier, deliberately:
#
#   * the tmux socket lives INSIDE the sandbox (`/mysbx-home/.mysbx-tmux/
#     socket`, in the home tmpfs), not in the repository's worktrees
#     sibling. Under mysbx a host-path socket would be reachable from
#     every other sandbox of the same repo, and a tmux socket is a
#     command-execution service — see D16, which also lists the guards
#     that keep it unshareable.
#   * `workmux add` needs the `<repo>__worktrees` sibling, which is
#     outside the mysbx repo mount: a sandbox that should create
#     worktrees declares that directory `rw` in its sidecar
#     `[[mounts]]`. Everything else (dashboard, sidebar, panes) works
#     without it.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.workmux.mysbx;
  wmCfg = config.myconfig.ai.workmux;
  aiCfg = config.myconfig.ai;

  # The workmux configuration used *inside* the mysbx sandbox. Like the
  # jail tier's `jailWorkmuxConfig` (./jail.nix), the `pi` named agent
  # must be the PLAIN binary: mysbx is already the sandbox, and the
  # host config's `pi` -> `pi-bwrap` would start a nested one with a
  # fresh tmpfs home, losing pi's configuration and credentials.
  #
  # The command is the bare name, resolved from the sandbox `PATH`:
  # ../programs/programs.pi-coding-agent adds pi to `myconfig.ai.mysbx.extraTools`
  # under the same condition, so the binary the sandbox has is the
  # binary this config names.
  sandboxAgents = lib.optionalAttrs aiCfg.pi-coding-agent.enable {
    pi = {
      type = "pi";
      command = "pi";
    };
  };
in
{
  options.myconfig.ai.workmux.mysbx = with lib; {
    enable = mkOption {
      type = types.bool;
      default = wmCfg.enable;
      defaultText = literalExpression "config.myconfig.ai.workmux.enable";
      description = ''
        Make the interactive payload of every `mysbx` sandbox a workmux
        tmux session (on a socket inside the sandbox — see
        ../mysbx/docs/design/config.md D16). Defaults to on wherever
        `myconfig.ai.workmux` is enabled; it only takes effect on hosts
        that also enable `myconfig.ai.mysbx`.
      '';
    };
  };

  config = lib.mkIf (wmCfg.enable && cfg.enable && aiCfg.mysbx.enable) {
    myconfig.ai.mysbx.workmux = {
      enable = true;
      inherit (wmCfg) package;
      # Everything but the agents is inherited verbatim from the host
      # settings (nerdfont, the `<agent>` pane layout, the default
      # agent), exactly as ./jail.nix and ./sandbox.nix do.
      settings = {
        agents = sandboxAgents;
      }
      // wmCfg.settings;
      # The host's tmux configuration (`programs.tmux` writes
      # `/etc/tmux.conf`), so the in-sandbox server keeps the same
      # keybindings and theme. `null` when the host has none.
      tmuxConf = config.environment.etc."tmux.conf".source or null;
    };
  };
}
