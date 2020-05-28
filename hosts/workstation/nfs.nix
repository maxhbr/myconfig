{ pkgs, lib, ...}:
{ config =
    { services.nfs =
        { server =
            { enable = true;
              exports = ''
                /data               192.168.178.0/24(rw,fsid=0,insecure,no_subtree_check,crossmnt,fsid=0)
                /data/data          192.168.178.0/24(rw,nohide,insecure,no_subtree_check)
                /data/guest         192.168.178.0/24(rw,nohide,insecure,no_subtree_check)
              '';
            };
          statdPort = 4000;
          lockdPort = 4001;
        };
      networking.firewall.allowedTCPPorts = [ 2049 111 4000 4001 19999 ];
      networking.firewall.allowedUDPPorts = [ 2049 111 4000 4001 19999 ];

      fileSystems."/export/data" = {
        device = "/mnt/data";
        options = [ "bind" ];
      };
      fileSystems."/export/guest" = {
        device = "/mnt/guest";
        options = [ "bind" ];
      };

      # Monitoring
      services.netdata =
        { enable = true;
          config =
            { global =
                { "default port" = "19999";
                  "bind to" = "*";
                  # 7 days
                  "history" = "604800";
                  "error log" = "syslog";
                  "debug log" = "syslog";
                };
            };
        };
      systemd.enableCgroupAccounting = true;
      services.vnstat.enable = true;
    };
}
