{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;
  startGnome = pkgs.writeShellScriptBin "start-gnome" ''
    dbus-run-session -- gnome-shell --display-server --wayland
  '';
in {
  options.myconfig = with lib; {
    desktop.wayland.gnome = { enable = mkEnableOption "gnome"; };
  };
  config =
    lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.gnome.enable) {
      services.desktopManager.gnome.enable = true;
      environment.gnome.excludePackages =
        (with pkgs; [ gnome-photos gnome-tour ]) ++ (with pkgs.gnome; [
          cheese # webcam tool
          gnome-music
          gnome-terminal
          gedit # text editor
          epiphany # web browser
          geary # email reader
          evince # document viewer
          gnome-characters
          totem # video player
          tali # poker game
          iagno # go game
          hitori # sudoku game
          atomix # puzzle game
        ]);
      home-manager.sharedModules = [{
        home.packages = (with pkgs.gnome3; [
          evince # pdf reader
          gnome-tweak-tool
        ]) + (with pkgs.gnomeExtensions; [
          startGnome
          true-color-invert
          miniview
          # material-shell
          forge
        ]);
      }];
    };
}
