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

  # Let systemd manage the mount on the *host* mount namespace and pull it in
  # on demand. Mounting by hand inside the service's preStart does NOT work:
  # the restic service runs with PrivateTmp=, so each Exec* phase gets its own
  # transient mount namespace. A mount done in preStart is discarded before
  # ExecStart runs, which previously made `restic init` (preStart) succeed but
  # `restic backup` (ExecStart) fail with "repository does not exist".
  #
  # RequiresMountsFor adds Requires=+After= on the auto-generated .mount unit
  # (no manual unit-name escaping needed) so the noauto/nofail mount is started
  # before the service. ReadWritePaths exposes the host mount inside the
  # service's namespace.
  systemd.services."restic-backups-restic" = {
    unitConfig.RequiresMountsFor = [ mnt ];
    serviceConfig.ReadWritePaths = [ mnt ];
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
      "--keep-monthly unlimited"
    ];

    # The USB HDD is mounted by systemd via RequiresMountsFor (see the
    # systemd.services."restic-backups-restic" block above) before this runs,
    # on the host mount namespace. This guard is defense in depth: if for any
    # reason ${mnt} is not actually a mountpoint, abort NON-ZERO so the whole
    # service fails before any restic command runs and nothing is ever written
    # to the root filesystem at ${repo}. The timer ignores service failures, so
    # a missing/unmountable drive does not block anything else.
    backupPrepareCommand = ''
      set -euo pipefail

      echo "[restic-prepare] mount point: ${mnt}"

      if ! ${pkgs.util-linux}/bin/mountpoint -q "${mnt}"; then
        echo "[restic-prepare] ${mnt} is not a mountpoint — the backup drive" >&2
        echo "[restic-prepare] (UUID ${uuid}) is not mounted." >&2
        echo "[restic-prepare] cause: USB HDD not plugged in / powered off, a" >&2
        echo "[restic-prepare]        changed filesystem UUID, or a mount failure." >&2
        echo "[restic-prepare] available disks/UUIDs for reference:" >&2
        ${pkgs.util-linux}/bin/lsblk -o NAME,SIZE,FSTYPE,UUID,MOUNTPOINT >&2 || true
        echo "[restic-prepare] aborting backup so restic never writes to the root fs." >&2
        exit 1
      fi

      echo "[restic-prepare] ${mnt} is mounted; repo path: ${repo}"
      mkdir -p "${repo}"
    '';
  };
}
