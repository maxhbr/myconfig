# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# USB HDD backup for host.nuc — covers the smart-home and observability stacks.
#
# The drive is mounted noauto/nofail; if it is absent the restic service will
# fail but the timer continues — no spurious failures block anything else.
# Run manually at any time: sudo systemctl start restic-backups-restic
# Browse snapshots:         restic-restic snapshots
# Mount latest:             restic-restic mount /mnt/restore
#
# Prerequisite (in priv/ repo): set the source for the secret stub:
#   myconfig.secrets."restic".source = ./path/to/plaintext-password;
#
# Paths and priorities:
#   /var/lib/hass             — critical (Home Assistant)
#   /var/lib/home-assistant   — critical (Home Assistant user home)
#   /var/lib/zigbee2mqtt      — critical (coordinator_backup.json holds Zigbee PAN/network
#                               key; without it all devices must be re-paired)
#   /var/lib/mosquitto        — low (no persistence configured)
#   /var/lib/grafana          — high (dashboards, user data)
#   /var/lib/loki/ruler       — high (alert rules; small)
#   /var/lib/victoriametrics  — large (90-day retention; re-scraped on restart)
#   /var/lib/loki/chunks      — large (30-day retention; logs roll off)
{
  config,
  lib,
  pkgs,
  ...
}:
let
  uuid = "e140f8b4-15ef-47c3-8058-a58d76473a4b";
  mnt = "/mnt/backup/${uuid}";
  repo = "${mnt}/restic/${config.networking.hostName}-smart-home-obs";
  passwordFile = "/etc/restic/password";
in
{
  # Secret stub — source is set in priv/. The dest path is stable so the restic
  # service and the restic-smart-home-obs wrapper always find it at the same location.
  myconfig.secrets."restic" = {
    dest = passwordFile; # /etc/restic/password
    # source = <set in priv/>;
  };

  fileSystems."${mnt}" = {
    device = "/dev/disk/by-uuid/${uuid}";
    fsType = "ext4";
    options = [ "noauto,nofail,x-systemd.device-timeout=1,sync,users,rw" ];
  };

  services.restic.backups.restic = {
    repository = repo;
    passwordFile = passwordFile;
    initialize = true; # safe: runs `restic cat config || restic init`

    paths = [
      "/var/lib/hass"
      "/var/lib/home-assistant"
      "/var/lib/zigbee2mqtt"
      "/var/lib/mosquitto"
      "/var/lib/grafana"
      "/var/lib/loki/ruler"
      # large initial backup; deduplicates well after first run:
      "/var/lib/victoriametrics"
      "/var/lib/loki/chunks"
    ];

    timerConfig = {
      OnCalendar = "03:00";
      Persistent = true; # catch up missed runs after e.g. a reboot
    };

    pruneOpts = [
      "--keep-within 1d"
      "--keep-daily 7"
      "--keep-weekly 4"
      "--keep-monthly -1"
    ];

    # Ensure the mount point exists and the drive is mounted before starting.
    # Exit cleanly when the drive is absent so the timer does not enter failed state.
    backupPrepareCommand = ''
      mkdir -p "${mnt}"
      if ! mount /dev/disk/by-uuid/${uuid} "${mnt}" 2>/dev/null; then
        # already mounted or mount failed — check if it's actually there
        if ! mountpoint -q "${mnt}"; then
          echo "Backup drive not available, skipping."
          exit 0
        fi
      fi
      mkdir -p "${repo}"
    '';

    backupCleanupCommand = ''
      umount "${mnt}" 2>/dev/null || true
    '';
  };
}
