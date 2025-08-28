# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig;
  user = myconfig.user;
in
{
  config = (
    lib.mkIf
      (cfg.desktop.wayland.enable && (builtins.elem "plasma6" cfg.desktop.wayland.selectedSessions))
      (
        lib.mkMerge [
          (lib.mkIf (builtins.elem "plasma6" cfg.desktop.wayland.selectedSessions) {
            services.desktopManager.plasma6.enable = true;
            environment.plasma6.excludePackages = with pkgs.kdePackages; [
              plasma-browser-integration
              konsole
              oxygen
            ];
            myconfig.desktop.wayland.sessions = {
              plasma6 = {
                command = "${pkgs.kdePackages.plasma-workspace}/bin/startplasma-wayland";
              };
            };
          })

          {
            environment.systemPackages = with pkgs.kdePackages; [
              kdeplasma-addons
              arianna
            ];
          }
          {
            home-manager.sharedModules = [
              (
                { config, ... }:
                {
                  xdg.configFile = {
                    "kwalletrc".text = ''
                      [Wallet]
                      # permanently disable kwallet
                      Enabled=false
                      First Use=false
                    '';
                  };
                  myconfig.persistence.directories = [
                    ".local/share/kwalletd"
                    ".local/share/kwalletmanager5"
                  ];
                }
              )
            ];
          }
        ]
      )
  );
}
