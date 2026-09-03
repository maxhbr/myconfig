# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ myconfig, ... }:

{
  # Creates a Forgejo API token for the hermes-agent user at boot and writes
  # it to /run/forgejo-hermes-agent-token for the agent to use (see
  # modules/myconfig.forgejo.client.nix).
  myconfig.forgejo.client = {
    enable = true;
    apiBase = "http://${myconfig.metadatalib.getWgIp "thing"}:3000";
    tokens = {
      "hermes-agent" = { };
    };
  };
}
