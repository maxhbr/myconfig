{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    (
      let
        deflux = pkgs.callPackage ./deflux { };
      in
      {
        config = {
          home-manager.sharedModules = [ { home.packages = [ deflux ]; } ];
          users.users.deflux = {
            group = "deflux";
            isSystemUser = true;
          };
          users.groups.deflux = { };
          environment.etc = {
            "deflux.yml".text = ''
              deconz:
                addr: http://127.0.0.1:${toString config.myconfig.services.deconz.httpPort}/api
                apikey: "CF1CAF4EDA"
              influxdb:
                addr: http://127.0.0.1:8086/
                useragent: Deflux
                # username: Deflux
                # password: Deflux
              influxdbdatabase: deconz
            '';
          };
          systemd.services.deflux = {
            wantedBy = [ "multi-user.target" ];
            after = [
              "network.target"
              "influxdb.target"
            ];
            description = "Start deflux.";
            serviceConfig = {
              Type = "simple";
              User = "deflux";
              ExecStart = "${deflux}/bin/deflux";
            };
          };
        };
      }
    )
  ];
  config = {
    home-manager.sharedModules = [ { home.packages = with pkgs; [ influxdb ]; } ];
    services = {
      influxdb = {
        enable = true;
      };
      grafana.provision = {
        enable = true;
        datasources = [
          {
            name = "influxdb";
            type = "influxdb";
            database = "influxdb";
            editable = false; # Force editing in this file.
            access = "proxy";
            # user = "grafana"; # fill in Grafana InfluxDB user, if enabled
            # password = "grafana";
            url = "http://127.0.0.1:8086";
          }
          {
            name = "influxdb-deconz";
            type = "influxdb";
            database = "deconz";
            editable = false; # Force editing in this file.
            access = "proxy";
            # user = "Deflux";
            # assword = "Deflux";
            url = "http://127.0.0.1:8086";
          }
        ];
      };
    };
  };
}
