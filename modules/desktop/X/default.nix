# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  mkstopscreensaver = with pkgs; writeScriptBin "myStopScreensaver.sh" ''
#!${stdenv.shell}
printf "run: "
while true; do
    printf "."
    sleep $((60 * 4))
    ${xdotool}/bin/xdotool key shift
done
  '';
in {
  imports = [
    ../common.nix
  ];

  options = {
    # option declarations
  };

  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        arandr
        xlibs.xmodmap xlibs.xset xlibs.setxkbmap
        xclip
        xdotool
      ];
      home.file = {
        ".fontconfig/fonts.conf".source = ./fontconfig/fonts.conf;
        ".config/zathura/zathurarc".source = ./config/zathura/zathurarc;
      };
      xresources.extraConfig = builtins.readFile ./Xresources;
    };

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
            ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name ${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ/cursors/left_ptr 128 &disown
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
      };
    };
  };
}
