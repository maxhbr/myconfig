# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# Host-specific agent user accounts on p14.  See
# modules/myconfig.agentUsers.nix for the shared definition and the
# features (ephemeral home, launch scripts, nix access, network
# isolation for "offline", …) each agent receives.
#
# `agnt-embedded` is the Zephyr/embedded-development agent: it receives
# device-access groups so west flash / pyocd / OpenOCD can reach the
# board. Everything else (ephemeral home, no secrets, nix
# allowed-but-untrusted) is inherited from the shared agent model.
# Flashing works from this user because it shares the host kernel —
# see modules/myconfig.ai.dev/docs/agent-sandboxing-tiers.README.md for why the VM sandbox tiers
# cannot pass USB/serial devices through.
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  config = {
    myconfig.agentUsers = {
      # NOTE: uid/gid are assigned positionally from 31000 (see the shared
      # module), so only ever *append* names here — reordering changes ids.
      # The `agnt-` prefix marks this as an agent-type user.
      names = [
        "agent"
        "agnt-embedded"
      ];

      # Device access for the embedded agent — the deliberate exception to
      # the "no extraGroups" agent model, restricted to device groups:
      #   dialout — /dev/ttyACM*, /dev/ttyUSB* (UART flashing, serial console)
      #   plugdev — USB debug probes (CMSIS-DAP, FTDI, J-Link, …) via the
      #              OpenOCD udev rules enabled below
      extraGroups.agnt-embedded = [
        "dialout"
        "plugdev"
      ];
    };

    # `plugdev` is declared by myconfig.dev.embedded (see
    # ../../modules/dev/dev.embedded.nix); the upstream OpenOCD probe
    # rules in its `services.udev.packages` make FT2232/CMSIS-DAP/…
    # USB probes GROUP="plugdev" MODE 660.
  };
}
