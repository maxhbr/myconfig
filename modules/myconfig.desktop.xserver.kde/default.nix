{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.xserver.kde = { enable = mkEnableOption "kde"; };
  };
  imports = [
    ./services.xserver.desktopManager.plasma5.nix
  ];
  config = (lib.mkIf
    (config.services.xserver.enable && cfg.desktop.xserver.kde.enable) {
      environment.plasma6.excludePackages = with pkgs.kdePackages; [
        plasma-browser-integration
        konsole
        oxygen
      ];

      services = {
        xserver = {
          displayManager.lightdm.enable = lib.mkForce false;
        };
        displayManager.sddm.enable = lib.mkForce true;
        desktopManager.plasma6.enable = true;
      };
    });
}

