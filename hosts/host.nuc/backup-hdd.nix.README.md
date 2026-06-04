# USB HDD Backup Plan for host.nuc

Covers the smart-home stack and observability stack.
Implementation uses `mkHddBackupJob` from `flake.lib.nix` — the same
borgbackup-to-USB-HDD pattern used by other hosts in this repo.

## Data to back up

| Service | Path | Priority |
|---|---|---|
| Home Assistant | `/var/lib/hass` | critical |
| Home Assistant user home | `/var/lib/home-assistant` | critical |
| Zigbee2MQTT | `/var/lib/zigbee2mqtt` | critical (`coordinator_backup.json`, `database.db`) |
| Mosquitto | `/var/lib/mosquitto` | low (no persistence configured) |
| Grafana | `/var/lib/grafana` | high (dashboards, user data) |
| VictoriaMetrics | `/var/lib/victoriametrics` | optional (large; re-scraped on restart) |
| Loki chunks/cache | `/var/lib/loki/chunks`, `/var/lib/loki/tsdb-cache` | optional (large; logs roll off) |
| Loki ruler state | `/var/lib/loki/ruler` | high (alert rules) |

## Implementation steps

### 1. Get the USB drive UUID

Plug the drive into the nuc, then:

```bash
lsblk -o NAME,UUID,FSTYPE,SIZE
# or: blkid /dev/sdX1
```

### 2. Set up agenix secrets

Add to the `priv/` repo overlay for host.nuc (pattern from `flake.lib.nix:294`):

```nix
myconfig.flake.lib.setupAsBorgbackupClient <backupkey-src> <passphrase-src>
```

This materialises `/etc/borgbackup/ssh_key` and `/etc/borgbackup/passphrase` via
agenix — the same credentials used by other hosts doing remote borgbackup jobs.

### 3. Create `hosts/host.nuc/backup-hdd.nix`

```nix
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ myconfig, config, lib, pkgs, ... }:
let
  usbUuid = "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"; # fill in from step 1
in
(myconfig.flake.lib.mkHddBackupJob "smart-home-obs" usbUuid {
  paths = [
    "/var/lib/hass"
    "/var/lib/home-assistant"
    "/var/lib/zigbee2mqtt"
    "/var/lib/mosquitto"
    "/var/lib/grafana"
    "/var/lib/loki/ruler"
    # optionally add for full metrics history:
    # "/var/lib/victoriametrics"
    # "/var/lib/loki/chunks"
  ];
  startAt = "03:00"; # daily at 3 AM
}) { inherit config lib pkgs; }
```

`mkHddBackupJob` (defined in `flake.lib.nix:324`) takes care of:
- declaring a `noauto,nofail,x-systemd.device-timeout=1` `fileSystems` entry
  for the USB device at `/mnt/backup/<uuid>`
- configuring `services.borgbackup.jobs` with `removableDevice = true`,
  `repokey-blake2` encryption, `auto,lzma` compression, and a prune schedule
  (keep all from last day, 7 daily, 4 weekly, all monthly)
- generating a `mkBackup-smart-home-obs@<uuid>` helper script that mounts the
  drive, runs the job, exports the repo key on first run, and tails the journal

### 4. Import in `hosts/host.nuc/default.nix`

```nix
imports = [
  ...
  ./backup-hdd.nix
];
```

## Scheduling behaviour

- The borgbackup systemd service runs daily at 03:00.
- `removableDevice = true` means it exits 0 silently if the USB drive is not
  plugged in — no spurious failures when the drive is stored offsite.
- To run manually at any time:
  ```bash
  mkBackup-smart-home-obs@<uuid>
  ```

## Size considerations

- **VictoriaMetrics** (`/var/lib/victoriametrics`): 90-day retention, can be
  several GB. Borgbackup deduplicates well so daily incrementals are small
  after the first run, but the initial backup will be large. Omit if disk
  space is tight — metrics history is not irreplaceable.
- **Loki chunks** (`/var/lib/loki/chunks`, `tsdb-cache`): log data with 30-day
  retention. Same trade-off as VictoriaMetrics. The `ruler` subdir (alert
  rules) is small and worth keeping.
- **Zigbee2MQTT** is small but irreplaceable: `coordinator_backup.json` holds
  the Zigbee PAN/network key; without it re-pairing all devices is required.
