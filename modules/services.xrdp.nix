{ config, lib, pkgs, ... }: {
  config = (lib.mkIf config.services.xrdp.enable {
    services.xserver.desktopManager.xfce.enable = lib.mkDefault true;
    services.xrdp = {
      defaultWindowManager = "${pkgs.xfce.xfce4-session}/bin/xfce4-session";
      openFirewall = true;
    };
  });
}
