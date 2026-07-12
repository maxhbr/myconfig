# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Hermes agent configuration for f13.
#
# The `enable = true` flag is set in the external priv flake (priv/), along
# with the secrets (env file). This file holds only the non-secret
# host-specific config: the microvm backend.
{
  config,
  lib,
  ...
}:
{
  config = {
    myconfig.ai.hermes = {
      microvm = {
        enable = true;
        autostart = true;
      };
    };
  };
}
