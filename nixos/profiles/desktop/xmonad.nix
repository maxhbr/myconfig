{ config, pkgs, ... }:
let
  unstable = (import <unstable> {});
in {
  environment.systemPackages = (with pkgs; [
    unstable.dmenu unstable.dzen2
     unclutter
    xss-lock
    libnotify # xfce.xfce4notifyd # notify-osd
  ]) ++ (with unstable.haskellPackages; [
    xmonad xmobar yeganesh
  ]);

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
}
