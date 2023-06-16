{ pkgs, ... }:
# ########################################################################
# /mnt/data
# see:
# - https://github.com/NixOS/nixpkgs/issues/75540
# - https://gist.github.com/MaxXor/ba1665f47d56c24018a943bb114640d7
{
  config = {
    systemd.packages = [ pkgs.systemd ];
    environment.etc.crypttab.text = ''
      2x4t-0 UUID=d33b8927-cffe-49db-a64a-868273c4120b /etc/cryptkey luks,noearly
      2x4t-1 UUID=1669a966-f851-4247-b473-1ebbccf53061 /etc/cryptkey luks,noearly
    '';
    fileSystems."/mnt/2x4t" = {
      device = "/dev/mapper/2x4t-0";
      fsType = "btrfs";
      options = [
        "defaults"
        "noatime"
        "compress=zstd"
        "subvol=@sub"
        "nofail"
        "x-systemd.requires-mounts-for=/mnt/.2x4t-1"
      ];
    };
    fileSystems."/mnt/.2x4t-1" = {
      device = "/dev/mapper/2x4t-1";
      fsType = "btrfs";
      options = [ "defaults" "noatime" "compress=zstd" "subvol=@sub" "nofail" ];
    };

    services.snapper = {
      configs = { "2x4t" = { SUBVOLUME = "/mnt/2x4t"; }; };
    };

    services.nfs.server = {
      enable = true;
      exports = ''
        /export        192.168.1.0/24(rw,fsid=0,insecure,no_subtree_check,crossmnt,fsid=0) 10.199.199.0/24(rw,fsid=0,insecure,no_subtree_check,crossmnt,fsid=0)
        /export/data   192.168.1.0/24(rw,nohide,insecure,no_subtree_check)                 10.199.199.0/24(rw,nohide,insecure,no_subtree_check)
        /export/bilder 192.168.1.0/24(rw,nohide,insecure,no_subtree_check)
        /export/guest  192.168.1.0/24(rw,nohide,insecure,no_subtree_check)                 10.199.199.0/24(rw,nohide,insecure,no_subtree_check)
      '';
      statdPort = 4000;
      lockdPort = 4001;
    };
    networking.firewall.allowedTCPPorts = [ 2049 111 4000 4001 ];
    networking.firewall.allowedUDPPorts = [ 2049 111 4000 4001 ];

    fileSystems."/export/data" = {
      device = "/mnt/2x4t/lvm-data";
      options = [ "bind" "x-systemd.requires-mounts-for=/mnt/2x4t" ];
    };
    fileSystems."/export/guest" = {
      device = "/mnt/2x4t/lvm-guest";
      options = [ "bind" "x-systemd.requires-mounts-for=/mnt/2x4t" ];
    };
    fileSystems."/export/bilder" = {
      device = "/mnt/2x4t/lvm-bilder";
      options = [ "bind" "x-systemd.requires-mounts-for=/mnt/2x4t" ];
    };
  };
}
