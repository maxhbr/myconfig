# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.workmux.mysbx — the `mysbx` counterpart of
# `myconfig.ai.dev.workmux.jail` (bubblewrap, `agent-bubblewrap-workmux-tmux`)
# and `myconfig.ai.dev.workmux.sandbox` (microVM, `agent-qemu-workmux-tmux`).
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
#     outside the mysbx repo mount: an interactive `mysbx` run creates it
#     when it is missing and binds it `rw` implicitly, and a
#     `--session NAME` run binds the session's own directory there.
#     Everything else (dashboard, sidebar, panes) works without it.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.workmux.mysbx;
  wmCfg = config.myconfig.ai.dev.workmux;
  aiCfg = config.myconfig.ai.dev;

  # The workmux configuration used *inside* the mysbx sandbox. Like the
  # jail tier's `jailWorkmuxConfig` (./jail.nix), a named agent
  # entry must name the PLAIN binary: mysbx is already the sandbox,
  # and the host config's jailed launcher would start a nested one
  # with a fresh tmpfs home, losing the agent's configuration and
  # credentials.
  #
  # The in-sandbox agent map is DERIVED from
  # `myconfig.ai.dev.mysbx.defaultAgent` (the sandbox carries only the
  # default agent — the host's other named agents are not in the
  # sandbox's `extraTools` closure). The command is the bare name,
  # resolved from the sandbox `PATH`: the module setting the option
  # also adds its package to `extraTools`, so the binary the sandbox
  # has is the binary this config names.
  defaultAgent = aiCfg.mysbx.defaultAgent or null;
  sandboxAgents = lib.optionalAttrs (defaultAgent != null) {
    ${defaultAgent.kind} = lib.filterAttrs (_: v: v != null) {
      type = defaultAgent.kind;
      command = defaultAgent.command;
      args = if defaultAgent.args == [ ] then null else defaultAgent.args;
    };
  };
in
{
  options.myconfig.ai.dev.workmux.mysbx = with lib; {
    enable = mkOption {
      type = types.bool;
      default = wmCfg.enable;
      defaultText = literalExpression "config.myconfig.ai.dev.workmux.enable";
      description = ''
        Make the interactive payload of every `mysbx` sandbox a workmux
        tmux session (on a socket inside the sandbox — see
        ../mysbx/docs/design/config.md D16). Defaults to on wherever
        `myconfig.ai.dev.workmux` is enabled; it only takes effect on hosts
        that also enable `myconfig.ai.dev.mysbx`.
      '';
    };
  };

  config = lib.mkIf (wmCfg.enable && cfg.enable && aiCfg.mysbx.enable) {
    myconfig.ai.dev.mysbx.workmux = {
      enable = true;
      inherit (wmCfg) package;
      # Everything but the agents is inherited verbatim from the host
      # settings (nerdfont, the `<agent>` pane layout), exactly as
      # ./jail.nix and ./sandbox.nix do. The derived `agent` key comes
      # LAST, so the mysbx tier wins over
      # `myconfig.ai.dev.workmux.settings.agent` for the in-sandbox
      # config — intended: the sandbox agent map carries only the
      # sandbox default agent.
      settings = {
        agents = sandboxAgents;
      }
      // wmCfg.settings
      // lib.optionalAttrs (defaultAgent != null) { agent = defaultAgent.kind; };
      # The host's tmux configuration (`programs.tmux` writes
      # `/etc/tmux.conf`), so the in-sandbox server keeps the same
      # keybindings and theme. `null` when the host has none.
      tmuxConf = config.environment.etc."tmux.conf".source or null;
    };
  };
}
