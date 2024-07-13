# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  config = (lib.mkIf (cfg.desktop.wayland.enable
    && builtins.elem "kde" cfg.desktop.wayland.selectedSessions) {
      # services.xserver.enable = true;
      # services.xserver.displayManager.sddm.enable = true;

      # # services.xserver.enable = lib.mkForce true;
      # services.xserver.displayManager.sddm.enable = false;
      # services.xserver.displayManager.gdm.enable = true;

      services.xserver.desktopManager.plasma5.enable = true;
      environment.systemPackages = with pkgs; [
        plasma5Packages.bismuth
        kdeconnect
        kdeplasma-addons
      ];
      myconfig.desktop.wayland.sessions = {
        kde = { command = "${pkgs.plasma-workspace}/bin/startplasma-wayland"; };
      };
      networking.firewall = {
        allowedTCPPortRanges = [ 
          { from = 1714; to = 1764; } # KDE Connect
        ];  
        allowedUDPPortRanges = [ 
          { from = 1714; to = 1764; } # KDE Connect
        ];
      };
    });
}
