# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# Host-specific agent user accounts on thing.  See
# modules/myconfig.agentUsers.nix for the shared definition and the
# features (ephemeral home, launch scripts, nix access, network
# isolation for "offline", …) each agent receives.
#
# `agnt-embedded` is the Zephyr/embedded-development agent: it receives
# device-access groups so west flash / pyocd / OpenOCD can reach the
# board. Everything else (ephemeral home, no secrets, nix
# allowed-but-untrusted) is inherited from the shared agent model.
# Flashing works from this user because it shares the host kernel —
# see modules/myconfig.ai/docs/README.md for why the VM sandbox tiers
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

    # `plugdev` has no NixOS default; the udev rules below assign probe
    # nodes to it (MODE 660, GROUP plugdev). Declaring the group makes
    # those rules effective.
    users.groups.plugdev = { };

    # Upstream OpenOCD probe rules (contrib/60-openocd.rules): FT2232/CMSIS-DAP/…
    # USB probes become GROUP="plugdev" MODE 660. Without them, the
    # `agnt-embedded` agent could only use serial-port flashing, not SWD/JTAG.
    services.udev.packages = [ pkgs.openocd ];
    # NOTE: J-Link probes need Segger's udev rules instead (not in
    # nixpkgs for licensing reasons); pyocd ships its own 70-pyocd.rules
    # with the toolchain flake — neither is wired up here yet.

    # Per-agent home-manager blocks for thing-specific customisation.
    # The shared module already activates home-manager for every agent
    # (via genAttrs); these empty blocks are anchors to fill in.
    home-manager.users = {
      agent = { };
      agnt-embedded = { };
    };
  };
}
