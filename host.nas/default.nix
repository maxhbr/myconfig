# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  imports = [
    # hardware:
    ./hardware-configuration.nix
    ../hardware/efi.nix
    ../hardware/hdd-spinndown.nix
    {
      boot.initrd.supportedFilesystems = [ "btrfs" "luks" ];

      services.btrfs.autoScrub = {
        enable = true;
        # fileSystems = [ "/" "/mnt/v0" ];
      };

      # ########################################################################
      # # USB HDDS:
      # fileSystems."/mnt/tng-backup" = {
      #   device = "/dev/disk/by-uuid/480aaf1f-aae0-469e-911e-13f961b46ec3";
      #   fsType = "ext4";
      #   options =
      #     [ "auto,nofail,x-systemd.device-timeout=1,users,rw,discard,noatime" ];
      # };
      # fileSystems."/mnt/foto-backup" = {
      #   device = "/dev/disk/by-uuid/a11523b0-e4f5-4af6-8551-2b43989c4781";
      #   fsType = "ext4";
      #   options =
      #     [ "auto,nofail,x-systemd.device-timeout=1,users,rw,discard,noatime" ];
      # };
    }
    ./2x4000-hdds.raid.nix
    # configuration
    ../role.headless
    ../role.smarthome
    ../role.dev/virtualization.docker
    ../role.imagework/exfat.nix
    # ../role.desktop/kiosk/headless.kiosk.nix
  ] ++ (with (import ../lib.nix); [ (setupAsWireguardClient "10.199.199.6") ]);
  config = {
    networking.hostName = "nas";
    networking.hostId = "29d93341";

    services.logind.extraConfig = ''
      HandlePowerKey=reboot
    '';
  };
}
