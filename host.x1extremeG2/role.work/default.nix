# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
let user = config.myconfig.user;
in {
  imports = [
    # ./jdk.nix
    # ./node.nix
    # ./evolution.nix
  ];
  config = {
    nixpkgs.overlays = map (n: import n) [
      ./idea-ultimate
      ./thrift011.nix
      ./thrift012.nix
      ./thrift93.nix
    ];
    home-manager.users."${user}" = {
      imports = [
        {
          home.packages = with pkgs; [
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
        }
        {
          home.packages = with pkgs; [
            openvpn
            networkmanager_openvpn
            openconnect
            networkmanager-openconnect
            strongswan
            networkmanager_strongswan
            networkmanagerapplet
          ];
        }
      ];
      home.packages = with pkgs; [
        idea-ultimate # jetbrains.phpstorm
        dia
        insync
        exiftool
        # misc-desktop-tools:
        libreoffice
        nixos-unstable-small.zoom-us
        bluejeans-gui
        slack
        element-desktop
        rambox
        remmina
      ];
    };
  };
}
