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
      "--keep-monthly unlimited"
    ];

    # Ensure the drive is mounted before starting. This script runs as a
    # *child* of the service preStart, so `exit 0` here would NOT prevent the
    # subsequent `restic init`/`restic backup` from running — it would only
    # return control to the parent preStart, which would then initialise and
    # write the repo on the *root* filesystem at ${repo}. We therefore exit
    # NON-ZERO when the drive is absent: that fails the whole service before
    # any restic command runs, so nothing is ever written to the wrong place.
    # The timer ignores service failures, so a missing drive does not block
    # anything else.
    backupPrepareCommand = ''
      set -euo pipefail

      dev="/dev/disk/by-uuid/${uuid}"
      echo "[restic-prepare] target device: $dev"
      echo "[restic-prepare] mount point:   ${mnt}"

      mkdir -p "${mnt}"

      # Already mounted? Then there is nothing to do.
      if ${pkgs.util-linux}/bin/mountpoint -q "${mnt}"; then
        echo "[restic-prepare] ${mnt} is already a mountpoint — reusing it."
      else
        # Is the block device even present? If the UUID does not resolve, the
        # USB HDD is unplugged / powered off / has a different filesystem UUID.
        if [ ! -e "$dev" ]; then
          echo "[restic-prepare] device $dev does not exist." >&2
          echo "[restic-prepare] cause: USB HDD not plugged in, not powered on," >&2
          echo "[restic-prepare]        or its filesystem UUID differs from ${uuid}." >&2
          echo "[restic-prepare] available disks/UUIDs for reference:" >&2
          ${pkgs.util-linux}/bin/lsblk -o NAME,SIZE,FSTYPE,UUID,MOUNTPOINT >&2 || true
          echo "[restic-prepare] aborting backup so restic never writes to the root fs." >&2
          exit 1
        fi

        # Device is present — try to mount it, capturing the real error message
        # from mount(8) instead of swallowing it.
        echo "[restic-prepare] mounting $dev at ${mnt} ..."
        if ! mount_err=$(mount "$dev" "${mnt}" 2>&1); then
          # A concurrent run or an already-present mount is fine; anything else
          # is a real failure we want explained in the journal.
          if ${pkgs.util-linux}/bin/mountpoint -q "${mnt}"; then
            echo "[restic-prepare] mount reported an error but ${mnt} is now mounted" \
                 "(likely a race with a concurrent run): $mount_err"
          else
            echo "[restic-prepare] mount failed: $mount_err" >&2
            echo "[restic-prepare] hints: filesystem dirty (needs fsck), wrong fsType" >&2
            echo "[restic-prepare]        (expected ext4), or device busy/read-only." >&2
            echo "[restic-prepare] aborting backup so restic never writes to the root fs." >&2
            exit 1
          fi
        fi
      fi

      # Final hard requirement: the USB HDD must actually be mounted here. If it
      # is not, abort so restic never writes to the root filesystem at ${repo}.
      if ! ${pkgs.util-linux}/bin/mountpoint -q "${mnt}"; then
        echo "[restic-prepare] ${mnt} is still not a mountpoint after mount attempt." >&2
        echo "[restic-prepare] aborting backup." >&2
        exit 1
      fi

      echo "[restic-prepare] ${mnt} is mounted; repo path: ${repo}"
      mkdir -p "${repo}"
    '';

    backupCleanupCommand = ''
      umount "${mnt}" 2>/dev/null || true
    '';
  };
}
