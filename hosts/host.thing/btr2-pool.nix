# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  myconfig,
  ...
}:
let
  btrfs_device = "/dev/disk/by-uuid/54e93b27-641e-484f-b8d5-6487b73bcf94";
in
{
  imports = [
    (
      { config, ... }:
      {
        config = lib.mkIf (config.services.vsftpd.enable) {
          # fileSystems."${config.users.extraUsers.ftpuser.home}" = {
          fileSystems."/var/ftp" = {
            device = btrfs_device;
            fsType = "btrfs";
            options = [
              "compress=zstd"
              "subvol=@ftp"
              "nofail"
              "discard"
              "noatime"
            ];
          };
        };
      }
    )
  ];
  config = {
    boot.initrd.luks.devices."btr2_pool" = {
      device = "/dev/disk/by-uuid/4162ab7a-c579-4152-9e50-98a819b0d0af";
      keyFile = "/dev/disk/by-id/usb-Verbatim_STORE_N_GO_25073013740484-0:0";
      keyFileSize = 4096;
      allowDiscards = true;
    };

    fileSystems."/btr2_pool" = {
      device = btrfs_device;
      fsType = "btrfs";
      options = [ "subvolid=5" ];
    };

    fileSystems."/home/${myconfig.user}/models" = {
      device = btrfs_device;
      fsType = "btrfs";
      options = [
        "compress=zstd"
        "subvol=@models"
        "nofail"
        "discard"
        "noatime"
      ];
    };

    fileSystems."/mnt/models" = {
      device = btrfs_device;
      fsType = "btrfs";
      options = [
        "ro"
        "compress=zstd"
        "subvol=@models"
        "nofail"
        "discard"
        "noatime"
      ];
    };

    fileSystems."/models" = {
      device = btrfs_device;
      fsType = "btrfs";
      options = [
        "ro"
        "compress=zstd"
        "subvol=@models"
        "nofail"
        "discard"
        "noatime"
      ];
    };

    fileSystems."/home/${myconfig.user}/imgwork2" = {
      device = btrfs_device;
      fsType = "btrfs";
      options = [
        "compress=zstd"
        "subvol=@imgwork"
        "nofail"
        "discard"
        "noatime"
      ];
    };

    services.nfs.server = {
      enable = true;
      exports = ''
        /home/${myconfig.user}/imgwork2 192.168.1.0/24(rw,nohide,insecure,no_subtree_check)
      '';
      # /home/${myconfig.user}/imgwork2 10.199.199.0/24(rw,nohide,insecure,no_subtree_check)
    };
    networking.firewall.interfaces."enp191s0" = {
      allowedTCPPorts = [
        111
        2049
      ];
      allowedUDPPorts = [
        111
        2049
      ];
    };
    # networking.firewall.interfaces."wg0" = {
    #   allowedTCPPorts = [ 111 2049 ];
    #   allowedUDPPorts = [ 111 2049 ];
    # };
  };
}
