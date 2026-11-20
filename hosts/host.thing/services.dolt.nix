# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Dolt SQL server as the bd (beads) backend. Clients connect in bd
# server mode from any wg0 peer (see README.md):
#   export BEADS_DOLT_SERVER_HOST=thing.wg0.maxhbr.local \
#          BEADS_DOLT_SERVER_PORT=3307
# which also marks the server externally managed, so bd never spawns its
# own. Migrating an existing embedded project:
#   bd backup init /tmp/beads-backup && bd backup sync
#   bd init --server && bd backup restore --force /tmp/beads-backup
# then on thing: cp -r .beads/dolt/myconfig /var/lib/dolt/myconfig
# (bd stores each server database in a subdirectory named after the
# project prefix).
{
  config,
  lib,
  myconfig,
  ...
}:
{
  services.dolt = {
    enable = true;
    host = myconfig.metadatalib.getWgIp config.networking.hostName;
    port = 3307;
  };

  # Allow peers to reach the Dolt SQL server on port 3307 via wg0.
  networking.firewall.interfaces."wg0".allowedTCPPorts = [
    config.services.dolt.port
  ];
}
