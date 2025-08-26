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
      system.nixos.tags = [ "headless" ];
      services.netdata.enable = true;
      services.vnstat.enable = true;
      services.vsftpd.enable = true;

      system.autoUpgrade.allowReboot = true;

      home-manager.sharedModules = [ { home.packages = with pkgs; [ vnstat ]; } ];
    }
  );
}
