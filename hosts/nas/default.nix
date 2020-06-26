# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
      { boot.initrd.supportedFilesystems = [ "btrfs" "luks" ];

        services.btrfs.autoScrub =
          { enable = true;
            # fileSystems = [ "/" "/mnt/v0" ];
          };

        ########################################################################
        # USB HDDS:
        fileSystems."/mnt/tng-backup" =
          { device = "/dev/disk/by-uuid/480aaf1f-aae0-469e-911e-13f961b46ec3";
            fsType = "ext4";
            options = ["auto,nofail,x-systemd.device-timeout=1,users,rw,discard,noatime"];
          };
        fileSystems."/mnt/foto-backup" =
          { device = "/dev/disk/by-uuid/a11523b0-e4f5-4af6-8551-2b43989c4781";
            fsType = "ext4";
            options = ["auto,nofail,x-systemd.device-timeout=1,users,rw,discard,noatime"];
          };
      }
    # ./4x500-hdds.raid.nix
    ./2x4000-hdds.raid.nix
    ../../roles/headless.nix
    ../../hardware/hdd-spinndown.nix
    ../../modules/service.monitoring.nix
    ../../modules/virtualization.docker
    ../../modules/service.syncthing.nix
    # hardware:
    ../../hardware/efi.nix
    # configuration
    ./docker.openhab.nix
    ../../modules/service.deconz.nix
    ../../modules/kiosk/headless.kiosk.nix
  ];
  config = {
    networking.hostName = "nas";
    networking.hostId = "29d93341";

    services.logind.extraConfig = ''
        HandlePowerKey=suspend
      '';

    local.services.deconz =
      let
        deconz = pkgs.qt5.callPackage ../../pkgs/deconz {};
      in
      { enable = true;
        package = deconz;
        device = "/dev/ttyACM0";
        wsPort = 9443;
        openFirewall = true;
        allowRebootSystem = false;
        allowRestartService = false;
        allowSetSystemTime = false;
      };

      services.snapper =
        { snapshotInterval = "hourly";
          cleanupInterval = "1d";
          filters = null;
          configs =
            { home =
                { subvolume = "/home";
                  extraConfig = ''
                    ALLOW_USERS="mhuber"
                  '';
                };
            };
        };
  };
}
