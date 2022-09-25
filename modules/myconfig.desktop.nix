{ config, lib, pkgs, ... }:

let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.enable = mkEnableOption "myconfig.desktop";
    desktop.flavor = mkOption {
      type = types.enum [ "xserver" "wayland" ];
      default = "wayland";
      description = "type of tesktop to use.";
    };

    # TODO: get rid of desktop.full
    desktop.full = mkEnableOption "myconfig.desktop.full" // {
      default = true;
      example = false;
    };
  };
  config = lib.mkIf cfg.desktop.enable {
    services.xserver.enable = cfg.desktop.flavor == "xserver";
    myconfig.wayland.enable = cfg.desktop.flavor == "wayland";

    home-manager.sharedModules = [
      {
        programs.firefox.enable = lib.mkDefault true;
        home.packages = with pkgs;
          [
            xarchiver
            sxiv
            kitty
            alacritty
            mupdf
            xarchiver
            feh
            imagemagick
            mplayer # unsuported on aarch
          ] ++ (with pkgs.nixos-unstable; [ tdesktop signal-desktop signal-cli ]);
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
