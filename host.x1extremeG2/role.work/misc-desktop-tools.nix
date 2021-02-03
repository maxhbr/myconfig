# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
let user = config.myconfig.user;
in {
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        libreoffice
        nixos-unstable-small.zoom-us
        bluejeans-gui
        slack
        rambox
        remmina
        nixos-2009-small.teams
      ];
      xdg.mimeApps = {
        defaultApplications."x-scheme-handler/msteams" = [ "teams.desktop" ];
      };
      programs.zsh.shellAliases = {
        unteams = ''while pkill teams; do echo "kill it with fire!"; done'';
      };
      programs.fish.functions = {
        unteams = ''
          while pkill teams
            echo "kill it with fire!"
          end
          echo "now wo are happy again"
        '';
      };
    };
  };
}
