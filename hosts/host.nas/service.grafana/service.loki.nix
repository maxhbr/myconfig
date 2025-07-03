{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = {
    services.loki = {
      enable = true;
      configFile = ./loki-local-config.yaml;
    };
    systemd.services.promtail = {
      description = "Promtail service for Loki";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        ExecStart = ''
          ${pkgs.grafana-loki}/bin/promtail --config.file ${./promtail.yaml}
        '';
      };
    };
  };
}
