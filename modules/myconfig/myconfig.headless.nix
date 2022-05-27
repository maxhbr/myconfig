{ config, lib, pkgs, ... }:

let
  cfg = config.myconfig;
  user = cfg.user;
in {
  options.myconfig = with lib; {
    headless.enable = mkEnableOption "headless / server-like stuff";
  };
  config = (lib.mkIf cfg.headless.enable {
    services.netdata.enable = true;
    services.vnstat.enable = true;
    services.vsftpd.enable = true;

    services.eternal-terminal = {
      enable = true;
      port = 22022;
    };
    networking.firewall.allowedTCPPorts = [ 22022 ];
    networking.firewall.allowedUDPPorts = [ 22022 ];

    systemd.enableCgroupAccounting = true;
    system.autoUpgrade.allowReboot = true;

    home-manager.sharedModules = [{ home.packages = with pkgs; [ vnstat ]; }];
  });
}
