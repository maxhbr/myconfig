# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Hermes agent configuration for f13.
#
# Only the CLI tier: hermes is on this host as a sandboxed payload (the
# `mysbx` tier of modules/myconfig.ai.dev/hermes-agent/mysbx.nix, pointed at
# the local LiteLLM forwarder), not as a gateway. The secrets for anything
# service-shaped live in the external priv flake (priv/) and are unused while
# `myconfig.ai.dev.hermes.service.enable` stays false.
#
# Enabling `service.microvm.enable` (or `service.container.enable`) here flips
# `microvm.host.enable` inside that module. Do NOT add a separate
# hosts/host.f13/agent-sandbox.nix that also imports
# `inputs.microvm.nixosModules.host` and blanket-enables
# `microvm.host.enable`/`updates-flake`: that bypasses the gating and loads
# tap/vhost_net + creates the `microvm` user unconditionally.
{
  config,
  ...
}:
{
  config.myconfig.ai.dev.hermes.enable = true;
}
