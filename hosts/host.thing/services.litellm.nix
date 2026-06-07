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
    # modules/myconfig.ai/services.litellm.nix.
    host = lib.mkOverride 49 "0.0.0.0";
    settings.router_settings = {
      model_group_alias = {
        "hermes" = "gfx1151:hermes";
        "hermes-fallback" = "gfx1151:hermes-fallback";
        "opencode-fast" = "rtx5090:opencode-fast";
        "opencode-fast-fallback" = "rtx5090:opencode-fast-fallback";
        "opencode" = "gfx1151:opencode";
        "opencode-slow" = "gfx1151:opencode-slow";
        "opencode-fallback" = "gfx1151:opencode-fallback";
        "sidekick" = "rtx5090:sidekick";
      };
    };
  };

  # Allow peers to reach LiteLLM on port 4000 via wg0.
  networking.firewall.interfaces."wg0".allowedTCPPorts = [
    config.services.litellm.port
  ];
}
