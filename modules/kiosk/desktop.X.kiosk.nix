# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# see also:
#  - https://gist.github.com/datakurre/cd29a985351e6b8c9bbc04532e5f9df0
{ pkgs, config, ... }:
{ imports =
    [ ./common.nix
    ];
  config =
    { services.xserver =
        { enable = true;
          config = ''
            Section "ServerFlags"
              Option  "DontVTSwitch"  "True"
            EndSection
          '';
          synaptics.enable = true;
          desktopManager.xterm.enable = false;
          windowManager.default = "i3";
          windowManager.i3.enable = true;
          windowManager.i3.configFile = pkgs.writeText "config" ''
            set $mod Mod4
            new_window 1pixel
            for_window [class="Firefox"] fullscreen
            exec ${pkgs.firefox}/bin/firefox "https://localhost:8443/habpanel/index.html#/view/Dashboard"
            exec ${pkgs.xdotool}/bin/xdotool search --sync --onlyvisible --class "Firefox" windowactivate key F11
            exec ${pkgs.xdotool}/bin/xdotool search --sync --onlyvisible --class "Firefox" windowactivate key F5
          '';

          displayManager =
            { lightdm =
                { enable = true;
                  autoLogin.enable = true;
                  autoLogin.user   = "kiosk";
                  # background = "${pkgs.my-wallpapers}/share/romben3.png";
                };
            };
        };
      environment.systemPackages = with pkgs; [
        i3status
        x11vnc
      ];
    };
}
