# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# Host-specific agent user accounts on f13.  See
# modules/myconfig.agentUsers.nix for the shared definition and the
# features (ephemeral home, launch scripts, nix access, …) each agent
# receives.
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
    ];
  };
}
