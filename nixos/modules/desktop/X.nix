# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./common.nix
  ];

  options = {
    # option declarations
  };

  config = {
    environment.systemPackages = with pkgs; [ xdotool ];
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
            background = "${pkgs.myconfig.background}/share/romben3.png";
          };
          sessionCommands = ''
            ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name ${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ/cursors/left_ptr 128 &disown
            if test -e $HOME/.Xresources; then
              ${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources &disown
            fi
            ${pkgs.myconfig.background}/bin/myRandomBackground &disown
            ${pkgs.xss-lock}/bin/xss-lock ${pkgs.myconfig.background}/bin/myScreenLock &disown
          '';
        };
      };

      cron = {
        enable = true;
        systemCronJobs = [
          "*/10 * * * *  mhuber ${pkgs.myconfig.background}/bin/myRandomBackground >> /tmp/cronout 2>&1"
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
