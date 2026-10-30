# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# Host-specific agent user accounts on f13.  See
# modules/myconfig.agentUsers.nix for the shared definition and the
# features (ephemeral home, launch scripts, nix access, network
# isolation for "offline", …) each agent receives.
{
  config,
  lib,
  myconfig,
  ...
}:
{
  config = {
    myconfig.agentUsers = {
      names = [
        "agent"
        "assistant"
        "offline"
      ];
      # assistant runs ThinkRail sessions on this host; keep its app
      # state across reboots (cache tree — survives reboots, not backed
      # up, same trade-off as the workdir).
      persistentDirectories.assistant = [ ".thinkrail" ];
    };

    # Per-agent home-manager blocks for f13-specific customisation.
    # The shared module already activates home-manager for every agent
    # (via genAttrs); these empty blocks are anchors to fill in.
    home-manager.users = {
      agent = { };
      assistant = { };
      offline = { };
    };
  };
}
