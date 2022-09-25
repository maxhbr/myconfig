{ config, lib, pkgs, ... }:

let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop= {
      enable = mkEnableOption "myconfig.desktop";
      wayland.enable = mkEnableOption "myconfig.desktop.wayland";
      xserver.enable = mkEnableOption "myconfig.desktop.xserver == !myconfig.desktop.wayland";

      # TODO: get rid of desktop.full
      full = mkEnableOption "myconfig.desktop.full" // {
        default = true;
        example = false;
      };
    };
  };
  config = lib.mkIf cfg.desktop.enable {
    myconfig.desktop.xserver.enable = ! cfg.desktop.wayland.enable;
    services.xserver.enable = ! cfg.desktop.wayland.enable;

    home-manager.sharedModules = [
      {
        programs.firefox.enable = lib.mkDefault true;
        home.packages = with pkgs;
          [
            libnotify
            xarchiver
            sxiv
            kitty
            alacritty
            mupdf
            xarchiver
            feh
            imagemagick
            mplayer # unsuported on aarch
          ];
        xdg.mimeApps = {
          enable = true;
          defaultApplications."image/jpeg" = [ "sxiv.desktop" ];
          defaultApplications."image/png" = [ "sxiv.desktop" ];
          defaultApplications."x-scheme-handler/slack" = [ "slack.desktop" ];
          defaultApplications."x-scheme-handler/zoommtg" =
            [ "us.zoom.Zoom.desktop" ];
        };
      }
    ];
    environment = {
      shellAliases = { file-roller = "${pkgs.xarchiver}/bin/xarchiver"; };
    };
  };
}
