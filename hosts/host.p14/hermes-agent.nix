# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Hermes agent configuration for p14 (aka "nuc").
#
# Home Assistant runs on this host (see ./homeassistant.nix), so hermes
# points at it via hassUrl. The `enable = true` and HASS config live here
# in the public repo; only the secrets (telegram token, env file) remain
# in the priv repo.
{
  config,
  lib,
  ...
}:
{
  config = {
    myconfig.ai.hermes = {
      enable = true;
      hassUrl = "http://hass.nuc.wg0.maxhbr.local";
    };
  };
}
