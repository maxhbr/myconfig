{ config, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.xmonadEnv ];

  services.xserver = {
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      default = "xmonad";
    };

    desktopManager = {
      xterm.enable = false;
      default = "none";
    };
  };

  # security.setuidPrograms = [ "slock" ];
}
