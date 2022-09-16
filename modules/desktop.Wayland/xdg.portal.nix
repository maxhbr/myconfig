{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;

  # currently, there is some friction between sway and gtk:
  # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
  # the suggested way to set gtk settings is with gsettings
  # for gsettings to work, we need to tell it where the schemas are
  # using the XDG_DATA_DIR environment variable
  # run at the end of sway config
  configure-gtk = pkgs.writeTextFile {
    name = "configure-gtk";
    destination = "/bin/configure-gtk";
    executable = true;
    text = let
      schema = pkgs.gsettings-desktop-schemas;
      datadir = "${schema}/share/gsettings-schemas/${schema.name}";
    in ''
      export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
      gnome_schema=org.gnome.desktop.interface
      ${pkgs.glib}/bin/gsettings set $gnome_schema gtk-theme 'Dracula'
    '';
  };

in {
  config = if cfg.wayland.enable && config.xdg.portal.enable then {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        configure-gtk
        dracula-theme # gtk theme
        gnome3.adwaita-icon-theme # default gnome cursors
      ];
    }];

    # xdg-desktop-portal works by exposing a series of D-Bus interfaces
    # known as portals under a well-known name
    # (org.freedesktop.portal.Desktop) and object path
    # (/org/freedesktop/portal/desktop).
    # The portal interfaces include APIs for file access, opening URIs,
    # printing and others.
    services.dbus.enable = true;
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      # gtk portal needed to make gtk apps happy
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      # warning: The option `xdg.portal.gtkUsePortal'has been deprecated. Setting the variable globally with `environment.sessionVariables' NixOS option can have unforseen side-effects.
      gtkUsePortal = true;
    };
  } else {
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [
          (writeShellScriptBin "configure-gtk" ''
            echo "nothing to do, as config.xdg.portal.enable=false"
          '')
        ];
    }];
  };
}
