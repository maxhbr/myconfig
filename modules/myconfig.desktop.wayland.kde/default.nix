# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  config = (lib.mkIf (cfg.desktop.wayland.enable
  && (builtins.elem "plasma5" cfg.desktop.wayland.selectedSessions ||
      builtins.elem "plasma6" cfg.desktop.wayland.selectedSessions)) (lib.mkMerge [
    (lib.mkIf (builtins.elem "plasma5" cfg.desktop.wayland.selectedSessions) {
      services.xserver.desktopManager.plasma5.enable = true;
      environment.systemPackages = with pkgs; [
        plasma5Packages.bismuth
      ];
      environment.plasma5.excludePackages = with pkgs.libsForQt5; [
        plasma-browser-integration
        konsole
        oxygen
      ];
      myconfig.desktop.wayland.sessions = {
        plasma5 = { command = "${pkgs.plasma-workspace}/bin/startplasma-wayland"; };
      };
    })
    (lib.mkIf (builtins.elem "plasma6" cfg.desktop.wayland.selectedSessions) {
      services.desktopManager.plasma6.enable = true;
      environment.plasma6.excludePackages = with pkgs.kdePackages; [
        plasma-browser-integration
        konsole
        oxygen
      ];
      myconfig.desktop.wayland.sessions = {
        plasma6 = { command = "${pkgs.plasma-workspace}/bin/startplasma-wayland"; };
      };
    })
    (lib.mkIf (builtins.elem "plasma5-bigsrceen" cfg.desktop.wayland.selectedSessions) {
      environment.systemPackages = with pkgs; [
        plasma5Packages.plasma-bigscreen
      ];
      myconfig.desktop.wayland.sessions = {
        plasma5-bigsrceen = { command = "${pkgs.plasma5Packages.plasma-bigscreen}/bin/plasma-bigscreen-wayland"; };
      };
    })
    (lib.mkIf config.programs.kdeconnect.enable {
      networking.firewall = {
        allowedTCPPortRanges = [ 
          { from = 1714; to = 1764; } # KDE Connect
        ];  
        allowedUDPPortRanges = [ 
          { from = 1714; to = 1764; } # KDE Connect
        ];
      };
    })
    {
      environment.systemPackages = with pkgs; [
        kdeplasma-addons
      ] ++ (with pkgs.kdePackages; [ arianna ]) ;
      programs.kdeconnect.enable = lib.mkDefault false;
    }
  ]));
}
