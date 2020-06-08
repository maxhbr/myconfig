{ pkgs, lib, ...}:
{ config =
    { services.nfs.server =
        { enable = true;
          exports = ''
            /export       192.168.178.0/24(rw,fsid=0,insecure,no_subtree_check,crossmnt,fsid=0) 10.199.199.0/24(rw,fsid=0,insecure,no_subtree_check,crossmnt,fsid=0)
            /export/data  192.168.178.0/24(rw,nohide,insecure,no_subtree_check)                 10.199.199.0/24(rw,nohide,insecure,no_subtree_check)
            /export/guest 192.168.178.0/24(rw,nohide,insecure,no_subtree_check)                 10.199.199.0/24(rw,nohide,insecure,no_subtree_check)
          '';
          statdPort = 4000;
          lockdPort = 4001;
        };
      networking.firewall.allowedTCPPorts = [ 2049 111 4000 4001 ];
      networking.firewall.allowedUDPPorts = [ 2049 111 4000 4001 ];

      fileSystems."/export/data" = {
        device = "/mnt/data0/lvm-data";
        options = [ "bind" "x-systemd.requires-mounts-for=/mnt/data0" ];
      };
      fileSystems."/export/guest" = {
        device = "/mnt/data0/lvm-guest";
        options = [ "bind" "x-systemd.requires-mounts-for=/mnt/data0" ];
      };
    };
}
