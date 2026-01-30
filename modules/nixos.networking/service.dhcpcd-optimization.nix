{
  config,
  lib,
  pkgs,
  ...
}:
{
  systemd.services.dhcpcd = {
    serviceConfig = {
      TimeoutStartSec = "5s";
    };
    serviceConfig.ExecStart = [
      "dhcpcd"
      "--quiet"
      "--config /etc/dhcpcd.conf"
      "--waitip"
    ];
  };
}
