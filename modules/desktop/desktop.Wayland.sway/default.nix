# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../desktop.common
  ];

  config = {
    home-manager.users.mhuber = {
      home.file = {
        ".config/sway/config".source = ./config/sway/config;
      };
      home.packages = with pkgs; [
        grim # for screenshots
        qt5.qtwayland
      ];
    };
    programs.sway = {
      enable = true;
      extraSessionCommands = ''
        # xrdb ~/.Xresources
        export SDL_VIDEODRIVER=wayland
        # needs qt5.qtwayland in systemPackages
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        # Fix for some Java AWT applications (e.g. Android Studio),
        # use this if they aren't displayed properly:
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    };
    environment = {
      loginShellInit = ''
        [[ -z $DISPLAY && $XDG_VTNR -eq 6 ]] && exec sway --my-next-gpu-wont-be-nvidia
      '';
    };
  };
}
