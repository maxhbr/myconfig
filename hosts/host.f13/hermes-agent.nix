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

  # NOTE (microvm host wiring): the microvm.nix *host* module is imported and
  # gated inside modules/myconfig.ai/hermes-agent/microvm.nix
  # (`microvm.host.enable`: mkDefault false, flipped true here). Do NOT add a
  # separate hosts/host.f13/agent-sandbox.nix that also imports
  # `inputs.microvm.nixosModules.host` and blanket-enables
  # `microvm.host.enable`/`updates-flake` — that would bypass the gating and
  # load tap/vhost_net + create the `microvm` user unconditionally. If a
  # reusable agent-sandbox host module is ever introduced, it must gate the
  # host module behind its own option with `lib.mkDefault false`, the same
  # pattern, not re-enable it globally.
}
