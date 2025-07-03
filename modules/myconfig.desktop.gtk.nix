# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig;

  iconTheme = {
    package = pkgs.numix-icon-theme-circle;
    name = "Numix-Circle";
  };
  theme =
    let
      light = false;
      gruvbox = {
        package = pkgs.gruvbox-gtk-theme;
        name = "Gruvbox-Dark-BL";
      };
      vimix = {
        package = pkgs.vimix-gtk-themes;
        name = if light then "vimix-light-doder" else "vimix-doder";
      };
      graphite = {
        package = pkgs.graphite-gtk-theme;
        name = if light then "Graphite-Light-hdpi" else "Graphite";
      };
    in
    graphite;

  # currently, there is some friction between sway and gtk:
  # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
  # the suggested way to set gtk settings is with gsettings
  # for gsettings to work, we need to tell it where the schemas are
  # using the XDG_DATA_DIR environment variable
  # run at the end of sway config
  configure-gtk =
    let
      schema = pkgs.gsettings-desktop-schemas;
      datadir = "${schema}/share/gsettings-schemas/${schema.name}";
    in
    pkgs.writeShellScriptBin "configure-gtk" ''
      export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
      gnome_schema=org.gnome.desktop.interface
      ${pkgs.glib}/bin/gsettings set $gnome_schema gtk-theme ${theme.name}
    '';

  # from https://github.com/riverwm/river/wiki
  extraCss = ''
    /* No (default) titlebar on wayland */
    headerbar.titlebar.default-decoration {
      background: transparent;
      padding: 0;
      margin: 0 0 -17px 0;
      border: 0;
      min-height: 0;
      font-size: 0;
      box-shadow: none;
    }

    /* rm -rf window shadows */
    window.csd,             /* gtk4? */
    window.csd decoration { /* gtk3 */
      box-shadow: none;
    }
  '';
in
{
  config = (
    lib.mkIf (cfg.desktop.wayland.enable || cfg.desktop.enable) {
      home-manager.sharedModules = [
        (
          { config, pkgs, ... }:
          {
            home.packages = with pkgs; [
              configure-gtk
              theme.package
              iconTheme.package
              adwaita-icon-theme # default gnome cursors
            ];
            gtk = {
              enable = true;
              inherit theme iconTheme;
              gtk3 = {
                inherit extraCss;
                bookmarks = [
                  "file:///tmp"
                  "file://${config.home.homeDirectory}"
                  "file://${config.home.homeDirectory}/tmp"
                  "file://${config.home.homeDirectory}/Downloads"
                  "file://${config.home.homeDirectory}/Documents"
                  "file://${config.home.homeDirectory}/MINE"
                  "file://${config.home.homeDirectory}/tng"
                ];
              };
              gtk4 = { inherit extraCss; };
            };
          }
        )
      ];
    }
  );
}
