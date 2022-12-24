# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  options.myconfig = with lib; {
    desktop.wayland.kde = { enable = mkEnableOption "kde"; };
  };
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.kde.enable) {
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
      myconfig.desktop.wayland.greetdSettings = {
        kde_session = {
          command = "${pkgs.plasma-workspace}/bin/startplasma-wayland";
          inherit user;
        };
      };
    });
}
