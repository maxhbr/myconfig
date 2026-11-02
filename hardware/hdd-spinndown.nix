# Copyright 2020-2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  pkgs,
  ...
}:
let
  spindownAllHdds = pkgs.writeShellApplication {
    name = "spindownAllHdds";
    runtimeInputs = with pkgs; [
      coreutils
      gawk
      hdparm
      util-linux
    ];
    text = ''
      # Usage: spindownAllHdds [<device>...]
      #
      # Set the APM level and the 20 minute standby timer on the given
      # devices, or on every rotational block device if none are given.
      # hdparm takes one device per call, and USB-SATA bridges often
      # refuse -B (APM) while accepting -S (standby timer), so report a
      # rejected device instead of aborting the whole run.
      spindown() {
          hdparm -S 240 -B 127 "$1" ||
              echo "spindownAllHdds: hdparm failed for $1" >&2
      }

      devices=("$@")
      if [[ ''${#devices[@]} -eq 0 ]]; then
          mapfile -t devices < <(lsblk -dnp -o name,rota | awk '$2 == 1 { print $1 }')
      fi

      if [[ ''${#devices[@]} -eq 0 ]]; then
          echo "spindownAllHdds: no rotational disks found" >&2
          exit 0
      fi

      for device in "''${devices[@]}"; do
          spindown "$device"
      done
    '';
  };

  serviceConfig = {
    Type = "oneshot";
    ExecStart = lib.getExe spindownAllHdds;
  };
in
{
  config = {
    systemd.services = {
      # Drives that are already spun down when the timer would have fired
      # forget it across a reboot, so re-arm on every boot.
      hdd-spindown = {
        description = "Spin down rotational HDDs (hdparm standby timer)";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = serviceConfig // {
          RemainAfterExit = true;
        };
      };

      # Same settings on suspend and on resume: a oneshot that sleep.target
      # pulls in runs its ExecStart while going to sleep and its ExecStop
      # once the system woke up again.
      hdd-spindown-sleep = {
        description = "Spin down rotational HDDs around system sleep";
        wantedBy = [ "sleep.target" ];
        before = [ "sleep.target" ];
        unitConfig.StopWhenUnneeded = true;
        serviceConfig = serviceConfig // {
          RemainAfterExit = true;
          ExecStop = lib.getExe spindownAllHdds;
        };
      };
    };
    home-manager.users.mhuber = {
      home.packages = [ spindownAllHdds ];
    };
  };
}
