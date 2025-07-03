{ pkgs, ... }:
# ########################################################################
# /mnt/4x500
# see:
# - https://github.com/NixOS/nixpkgs/issues/75540
# - https://gist.github.com/MaxXor/ba1665f47d56c24018a943bb114640d7
{
  config = {
    systemd.packages = [ pkgs.systemd ];
    environment.etc.crypttab.text = ''
      4x500-0 UUID=f435b742-75da-4a0b-894f-06f329afebb8 /etc/cryptkey luks,noearly
      4x500-1 UUID=327a652d-79b0-4740-950c-684d5b56e66d /etc/cryptkey luks,noearly
      4x500-2 UUID=da356132-803e-439b-886d-91d70048b7f9 /etc/cryptkey luks,noearly
      4x500-3 UUID=c5e77337-21ad-4338-987a-710201dd5636 /etc/cryptkey luks,noearly
    '';
    fileSystems."/mnt/4x500" = {
      device = "/dev/mapper/4x500-0";
      fsType = "btrfs";
      options = [
        "defaults"
        "noatime"
        "compress=zstd"
        "subvol=@sub"
        "nofail"
        "x-systemd.requires-mounts-for=/mnt/.4x500-3"
      ];
    };
    fileSystems."/mnt/.4x500-1" = {
      device = "/dev/mapper/4x500-1";
      fsType = "btrfs";
      options = [
        "defaults"
        "noatime"
        "compress=zstd"
        "subvol=@sub"
        "nofail"
      ];
    };
    fileSystems."/mnt/.4x500-2" = {
      device = "/dev/mapper/4x500-2";
      fsType = "btrfs";
      options = [
        "defaults"
        "noatime"
        "compress=zstd"
        "subvol=@sub"
        "nofail"
        "x-systemd.requires-mounts-for=/mnt/.4x500-1"
      ];
    };
    fileSystems."/mnt/.4x500-3" = {
      device = "/dev/mapper/4x500-3";
      fsType = "btrfs";
      options = [
        "defaults"
        "noatime"
        "compress=zstd"
        "subvol=@sub"
        "nofail"
        "x-systemd.requires-mounts-for=/mnt/.4x500-2"
      ];
    };

    services.nfs.server = {
      enable = true;
      exports = ''
        /export       192.168.1.0/24(rw,fsid=0,insecure,no_subtree_check,crossmnt,fsid=0)
        /export/data  192.168.1.0/24(rw,nohide,insecure,no_subtree_check)
        /export/guest 192.168.1.0/24(rw,nohide,insecure,no_subtree_check)
      '';
      statdPort = 4000;
      lockdPort = 4001;
    };
    networking.firewall.allowedTCPPorts = [
      2049
      111
      4000
      4001
    ];
    networking.firewall.allowedUDPPorts = [
      2049
      111
      4000
      4001
    ];

    fileSystems."/export/data" = {
      device = "/mnt/4x500/lvm-guest";
      options = [
        "bind"
        "x-systemd.requires-mounts-for=/mnt/4x500"
      ];
    };
    fileSystems."/export/guest" = {
      device = "/mnt/4x500/lvm-data";
      options = [
        "bind"
        "x-systemd.requires-mounts-for=/mnt/4x500"
      ];
    };
  };
}
