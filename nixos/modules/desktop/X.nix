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
            background = "${pkgs.my-backgrounds}/share/romben3.png";
          };
          sessionCommands = ''
            ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name ${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ/cursors/left_ptr 128 &disown
            if test -e $HOME/.Xresources; then
              ${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources &disown
            fi
            ${pkgs.my-backgrounds}/bin/myRandomBackground &disown
            ${pkgs.xss-lock}/bin/xss-lock ${pkgs.my-backgrounds}/bin/myScreenLock &disown
          '';
        };
      };

      cron = {
        enable = true;
        systemCronJobs = [
          "*/10 * * * *  mhuber ${pkgs.my-backgrounds}/bin/myRandomBackground >> /tmp/cronout 2>&1"
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
