# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  myconfig,
  ...
}:
{
  myconfig.ai.orca = {
    enable = true;
    # Use thing's wg0 address so that remote clients on the wireguard
    # network can reach the Orca pairing endpoint.
    pairingAddress = myconfig.metadatalib.getWgIp "thing";
  };

  # Allow peers to reach Orca on port 6768 via wg0.
  networking.firewall.interfaces."wg0".allowedTCPPorts = [
    config.myconfig.ai.orca.port
  ];
}
