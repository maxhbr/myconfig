# Copyright 2016-2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, ... }:
{
  services.litellm = {
    enable = true;
    # Bind LiteLLM to all interfaces so peers (e.g. p14, f13) can reach
    # http://<thing-wg-ip>:4000/v1 directly. The host itself (Caddy,
    # open-webui, vmagent, …) reaches it through `localhost:4000`. The
    # firewall rule below restricts external exposure to wg0. This
    # overrides the 127.0.0.1 lib.mkForce in
    # modules/myconfig.ai.llmops/services.litellm.nix.
    host = lib.mkOverride 49 "0.0.0.0";
  };

  # Allow peers to reach LiteLLM on port 4000 via wg0.
  networking.firewall.interfaces."wg0".allowedTCPPorts = [
    config.services.litellm.port
  ];
}
