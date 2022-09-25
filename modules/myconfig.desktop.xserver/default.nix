{ config, lib, pkgs, ... }:
let
  myInvert = with pkgs;
    writeScriptBin "myInvert" ''
      #!${stdenv.shell}
      ${systemd}/bin/systemctl --user stop redshift
      ${xrandr-invert-colors}/bin/xrandr-invert-colors
    '';
in {
  imports = [
    ./modules/services.xserver.autorandr.nix
    ./modules/services.xserver.mkscreenshot.nix
    ./modules/services.xserver.programs.xss-lock.nix
    ./modules/services.xserver.xclip.nix
    ./modules/services.xserver.kernel.nix
  ];
  config = lib.mkIf config.myconfig.desktop.xserver.enable {
    services = {
      xserver = {
        enable = true;
        autorun = true;
        enableCtrlAltBackspace = true;
        displayManager.lightdm.enable = false;
        displayManager.sddm = {
          enable = false;
          # wayland = true;
        };
      };
      redshift.enable = config.myconfig.desktop.full;
    };

    home-manager.sharedModules = [{
      services.dunst.enable = true;
      home.packages = with pkgs;
        [
          arandr
          xorg.xmodmap
          xorg.xset
          xorg.setxkbmap
          xorg.xkill
          xorg.xmessage
          xclip
          xdotool
          xrandr-invert-colors
          myInvert

          xrestop

          # misc
          libnotify # xfce.xfce4notifyd # notify-osd
        ];
      xresources.extraConfig = ''
        *utf8: 1

        !! Xft
        Xft.autohint: 0
        Xft.lcdfilter: lcddefault
        Xft.hintstyle: hintfull
        Xft.hinting: 1
        Xft.antialias: 1
        Xft.rgba: rgb

        !! Xterm
        xterm*loginShell: true
      '';
      xdg.mimeApps = {
        enable = true;
        defaultApplications."image/jpeg" = [ "sxiv.desktop" ];
        defaultApplications."image/png" = [ "sxiv.desktop" ];
        defaultApplications."x-scheme-handler/slack" = [ "slack.desktop" ];
        defaultApplications."x-scheme-handler/zoommtg" =
          [ "us.zoom.Zoom.desktop" ];
      };
    }];
    environment = {
      variables = { QT_AUTO_SCREEN_SCALE_FACTOR = "0"; };

      interactiveShellInit = ''
        xclipToX() {
          ${pkgs.xclip}/bin/xclip <(${pkgs.xclip}/bin/xclip -selection c -o)
        }

        xclipToCtrl() {
          ${pkgs.xclip}/bin/xclip -selection c <(${pkgs.xclip}/bin/xclip -o)
        }
      '';
    };
  };
}
