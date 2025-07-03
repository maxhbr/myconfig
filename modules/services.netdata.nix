{
  pkgs,
  config,
  lib,
  ...
}:
{
  config = (
    lib.mkIf config.services.netdata.enable {
      services.netdata = {
        config = {
          global = {
            "default port" = "19999";
            "bind to" = "*";
            # 7 days
            "history" = "604800";
            "error log" = "syslog";
            "debug log" = "syslog";
          };
        };
      };
      # use ssh port forwarding instead ;)
      # networking.firewall.allowedTCPPorts = [ 19999 ];
      # networking.firewall.allowedUDPPorts = [ 19999 ];
    }
  );
}
