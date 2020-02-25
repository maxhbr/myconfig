# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  myStopScreensaver = with pkgs; writeScriptBin "myStopScreensaver" ''
    #!${stdenv.shell}
    printf "run: "
    while true; do
        printf "."
        sleep $((60 * 4))
        ${xdotool}/bin/xdotool key shift
    done
  '';
  myInvert = with pkgs; writeScriptBin "myInvert" ''
    #!${stdenv.shell}
    ${pkgs.systemd}/bin/systemctl --user stop redshift
    ${xrandr-invert-colors}/bin/xrandr-invert-colors
  '';
  mySetBrightness = with pkgs; writeScriptBin "mySetBrightness" ''
    #!${stdenv.shell}
    ${pkgs.systemd}/bin/systemctl --user stop redshift
    ${pkgs.xorg.xrandr}/bin/xrandr --output DP-2 --brightness $${1:-1}
  '';
in {
  imports = [
    ../common
    ./big-cursor.nix
    ./autorandr
  ];

  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        arandr
        xlibs.xmodmap xlibs.xset xlibs.setxkbmap
        xclip
        xdotool
        myStopScreensaver
        xrandr-invert-colors myInvert
        mySetBrightness
      ];
      home.file = {
        ".fontconfig/fonts.conf".source = ./fontconfig/fonts.conf;
        ".config/zathura/zathurarc".source = ./config/zathura/zathurarc;
      };
      xresources.extraConfig = builtins.readFile ./Xresources;
    };

    environment.interactiveShellInit = ''
      xclipToX() {
        ${pkgs.xclip}/bin/xclip <(${pkgs.xclip}/bin/xclip -selection c -o)
      }

      xclipToCtrl() {
        ${pkgs.xclip}/bin/xclip -selection c <(${pkgs.xclip}/bin/xclip -o)
      }
    '';

    services = {
      xserver = {
        enable = true;
        autorun = true;
        layout = "de";
        xkbVariant = "neo";
        xkbOptions = "altwin:swap_alt_win";
        enableCtrlAltBackspace = true;

        displayManager = {
          lightdm = {
            enable = true;
            # autologin.user = "mhuber";
            background = "${pkgs.my-wallpapers}/share/romben3.png";
          };
          sessionCommands = ''
            if test -e $HOME/.Xresources; then
              ${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources &disown
            fi
            ${pkgs.my-wallpapers}/bin/myRandomBackground &disown
            ${pkgs.xss-lock}/bin/xss-lock ${pkgs.my-wallpapers}/bin/myScreenLock &disown
          '';
        };
      };

      cron = {
        enable = true;
        systemCronJobs = [
          "*/10 * * * *  mhuber ${pkgs.my-wallpapers}/bin/myRandomBackground >> /tmp/cronout 2>&1"
        ];
      };

      redshift = {
        enable = true;
        # temperature.day = 5500;
        # temperature.night = 3500;
        # brightness.day = "0.8";
        # brightness.night = "0.5";
      };
    };
  };
}
