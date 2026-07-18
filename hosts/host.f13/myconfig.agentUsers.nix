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
    myconfig.agentUsers = [
      "agent"
      "assistant"
      "offline"
    ];

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
