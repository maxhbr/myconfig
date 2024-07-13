# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  imports = [
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
  ];
  config = (lib.mkIf (cfg.desktop.wayland.enable
    && builtins.elem "kde" cfg.desktop.wayland.selectedSessions) {
      # services.xserver.enable = true;
      # services.xserver.displayManager.sddm.enable = true;

      # # services.xserver.enable = lib.mkForce true;
      # services.xserver.displayManager.sddm.enable = false;
      # services.xserver.displayManager.gdm.enable = true;

      # services.xserver.desktopManager.plasma5.enable = true;
      services.desktopManager.plasma6.enable = true;
      environment.systemPackages = with pkgs; [
        plasma5Packages.bismuth
        kdeconnect
        kdeplasma-addons
      ];
      environment.plasma5.excludePackages = with pkgs.libsForQt5; [
        plasma-browser-integration
        konsole
        oxygen
      ];
      environment.plasma6.excludePackages = with pkgs.kdePackages; [
        plasma-browser-integration
        konsole
        oxygen
      ];
      myconfig.desktop.wayland.sessions = {
        kde = { command = "${pkgs.plasma-workspace}/bin/startplasma-wayland"; };
      };
      programs.kdeconnect.enable = false;
    });
}
