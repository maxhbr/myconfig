# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The canonical set of `~/.pi/agent` subtrees a mysbx sandbox mounts, as
# relative paths under `$HOME`. A plain function file (no module
# machinery), consumed like `../../../mysbx/nix/sandbox-config.nix` — the
# single source of truth for the agents that read pi's configuration
# store:
#
#   * programs.pi-coding-agent/default.nix — pi itself;
#   * programs.thinkrail.nix — ThinkRail embeds the pi engine host, whose
#     `DefaultResourceLoader` uses `agentDir = getAgentDir()`
#     (`~/.pi/agent`), so it reads exactly this configuration.
#
# The list lives here, not in either module, because both mount into the
# SAME in-sandbox dests. Two independently maintained lists drift, and a
# second bind on one dest silently wins in every mysbx backend
# (`../../../mysbx/mysbx-rs/src/nono.rs`: "a later bind on the same dest
# wins"), so the sandbox payload would see a MIXTURE of two store trees
# with the winner decided by module merge order.
#
# `.pi/agent/mcp.json` is a FILE subtree: the helper copies it as a file,
# and its content is provided per-caller via `extraFiles` — never
# `home.file` — because the host's mcp.json stays runtime-managed via
# `pi mcp add` (see the mcp entry in the `subtrees` list below).
#
# Callers still decide WHO mounts (see the `mounts` gating in
# `programs.thinkrail.nix`); the shared list only decides WHAT. Rendering
# it through `mkSandboxConfig` stays per-caller so each derivation keeps
# its own name and `extraFiles`.
{
  config,
  lib,
  pkgs,
}:
let
  # The gate, the target path and the rendered content of pi's
  # user-level MCP registry mount come from ONE shared generator
  # (./pi-mcp-servers.nix): a private spelling of any of the three here
  # would let `subtrees` list a dest whose `extraFiles` entry (provided
  # per-caller, see the header) does not exist, which makes the helper
  # materialize an EMPTY DIRECTORY named `mcp.json` — a file-name bind
  # whose source is a directory, which pi fails to parse.
  piMcp = import ./pi-mcp-servers.nix { inherit config lib pkgs; };

  # `~/.agents/skills/` carries the handcrafted skills (plus, when
  # `myconfig.ai.dev.skills.enable` is on, the framework-synced selection)
  # for pi, which discovers that directory per the Agent Skills spec. Gate
  # the mount on the registry/framework being non-empty so a host with no
  # skills does not bind a missing path.
  #
  # `.pi/agent/skills` is deliberately NOT a subtree: `skills/default.nix`
  # maps pi to the `agents` target precisely so a second synced copy
  # cannot make pi discover every skill twice, and nothing deploys that
  # path (the helper would mount an empty directory).
  hasHandcraftedSkills =
    (config.myconfig.ai.dev.skills.handcrafted or { }) != { }
    || (config.myconfig.ai.dev.skills.enable or false);

  # Sandbox-only content (never `home.file`): pi's `project_trust`
  # handler that answers the trust prompt per-process for jailed
  # sessions. Declared here, next to the subtrees, because the check
  # (../../../mysbx/nix/checks.nix) must assert the same target path the
  # pi module mounts — two pinned spellings drift apart silently.
  sandboxOnlyFiles = [ ".pi/agent/extensions/myconfig-mysbx-trust.ts" ];
in
{
  # Relative paths under `$HOME` that get mounted into the sandbox.
  subtrees = [
    # Generated + example extensions (myconfig-providers.ts,
    # myconfig-jail-marker.ts, handoff.ts, subagent/, rtk.ts,
    # workmux-status.ts) — always deployed by the pi module's `home.file`
    # block or by the rtk/workmux modules.
    ".pi/agent/extensions"
    # Sub-agent definitions (upstream samples + handcrafted ones).
    ".pi/agent/agents"
    # Workflow prompt templates (`/implement`, `/commit`, …).
    ".pi/agent/prompts"
    # Themes (`unjailed.json`).
    ".pi/agent/themes"
    # Keybinding overrides (keybindings.json). A TUI concern; inert for a
    # headless engine host, and mounted anyway so both callers bind the
    # same set.
    ".pi/agent/keybindings.json"
    # pi's user-level MCP server registry, rendered from
    # home-manager's `programs.mcp.servers` (the shared generator).
    # A FILE subtree: the helper copies it as a file, not a directory —
    # its content is provided per-caller via `extraFiles`, never
    # `home.file` (the host's mcp.json stays runtime-managed via
    # `pi mcp add`).
  ]
  # The global AGENTS.md (../../../modules/myconfig.ai.dev/agentsMd), deployed through a `home.file`
  # entry when the feature is on. Not mounted otherwise: the helper
  # materialises an EMPTY DIRECTORY for a subtree nothing deployed into,
  # which would shadow the file name inside the sandbox.
  ++ lib.optional (config.myconfig.ai.dev.agentsMd.enable or false) ".pi/agent/AGENTS.md"
  # pi's mcp.json — mounted exactly when the registry rendered by the
  # shared generator is non-empty. Listed here so both callers bind it,
  # with the content from their own `extraFiles`.
  ++ lib.optional piMcp.enable piMcp.path
  # Handcrafted skills, discovered by pi from `~/.agents/skills/`.
  ++ lib.optional hasHandcraftedSkills ".agents/skills";

  inherit sandboxOnlyFiles;
}
