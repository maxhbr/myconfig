# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The global AGENTS.md: repository-independent instructions for the enabled
# agent harnesses, deployed on every host with `myconfig.ai.dev`.
#
# The content lives in ./AGENTS.md, and every harness renders this one text
# into its own global context location:
#
#   * pi        `~/.pi/agent/AGENTS.md`        (`home.file`, like every other
#               pi file this repo deploys: the pi module installs the binary
#               itself instead of going through `programs.pi-coding-agent`)
#   * opencode  `~/.config/opencode/AGENTS.md` (`programs.opencode.context`)
#   * claude    `~/.claude/CLAUDE.md`          (`programs.claude-code.context`;
#               Claude Code reads `CLAUDE.md`, not `AGENTS.md`)
#
# Harnesses discover the file per directory (pi: agent dir, cwd, ancestors;
# opencode: config dir), so one file per harness is enough; a project-level
# `AGENTS.md`/`CLAUDE.md` inside a repository stays the more specific context
# and wins.
#
# The sandbox tiers that stage or mount host configuration (mysbx, the qemu
# and microvm guests) carry the file through their existing allowlists and
# mount subtrees, which the program modules spell out.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.dev.agentsMd;

  # Referenced as a path so the file becomes a build input of the deployed
  # `home.file` entry.
  agentsMd = ./AGENTS.md;
in
{
  options.myconfig.ai.dev.agentsMd = with lib; {
    enable = mkEnableOption "the global AGENTS.md deployed to every enabled agent harness";

    context = mkOption {
      type = types.lines;
      default = builtins.readFile agentsMd;
      defaultText = literalExpression "builtins.readFile ./AGENTS.md";
      description = ''
        Content of the global `AGENTS.md`. Defaults to the shipped
        `./AGENTS.md`; a host may extend or replace it with its own text.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      (
        { lib, ... }:
        let
          harnessEnabled = {
            pi = config.myconfig.ai.dev.pi-coding-agent.enable or false;
            opencode = config.myconfig.ai.dev.opencode.enable or false;
            claude = config.myconfig.ai.dev.claude-code.enable or false;
          };
        in
        {
          # pi discovers `AGENTS.md` in its agent dir. Deployed as a plain
          # `home.file` entry (not `programs.pi-coding-agent.context`), the
          # same way this repo deploys every other file under `~/.pi`.
          home.file = lib.mkIf harnessEnabled.pi {
            ".pi/agent/AGENTS.md".text = cfg.context;
          };
          programs.opencode.context = lib.mkIf harnessEnabled.opencode cfg.context;
          # Claude Code's global context file is `CLAUDE.md`; the home-manager
          # module writes it to `${configDir}/CLAUDE.md`.
          programs.claude-code.context = lib.mkIf harnessEnabled.claude cfg.context;
        }
      )
    ];
  };
}
