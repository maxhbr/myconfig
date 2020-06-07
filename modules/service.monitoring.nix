{ pkgs, lib, ...}:
{ config =
    { services.netdata =
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
      networking.firewall.allowedTCPPorts = [ 19999 ];
      networking.firewall.allowedUDPPorts = [ 19999 ];
    };
}
