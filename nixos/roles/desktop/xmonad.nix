{ config, lib, pkgs, ... }:
let
  unstable = (import <unstable> {});
in {
  options = {
    myconfig.roles.xmonad = {
      enable = lib.mkEnableOption "Xmonad Desktop environment";
    };
  };

  config = lib.mkIf config.myconfig.roles.xmonad.enable {
    # myconfig.roles.desktop.enable = true;
    environment.systemPackages = (with pkgs; [
      unstable.dmenu unstable.dzen2
       unclutter
      xss-lock
      libnotify # xfce.xfce4notifyd # notify-osd
    ]) ++ (with unstable.haskellPackages; [
      xmonad xmobar yeganesh
    ]);

    system.activationScripts.cleanupXmonadState =
    ''
      rm /home/mhuber/.xmonad/xmonad.state || true
    '';

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
  };
}
