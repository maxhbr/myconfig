{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myconfig;
  user = cfg.user;
in
{
  options.myconfig = with lib; {
    headless.enable = mkEnableOption "headless / server-like stuff";
  };
  config = (
    lib.mkIf cfg.headless.enable {
      services.netdata.enable = true;
      services.vnstat.enable = true;
      services.vsftpd.enable = true;

      systemd.enableCgroupAccounting = true;
      system.autoUpgrade.allowReboot = true;

      home-manager.sharedModules = [ { home.packages = with pkgs; [ vnstat ]; } ];
    }
  );
}
