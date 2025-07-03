{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    myconfig.services.deconz = {
      enable = true;
      device = "/dev/ttyACM0";
      wsPort = 9443;
      openFirewall = true;
      allowRebootSystem = false;
      allowRestartService = false;
      allowSetSystemTime = false;
    };
  };
}
