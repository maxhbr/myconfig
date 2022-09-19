{ config, lib, pkgs, ... }:

# https://nixos.wiki/wiki/Sway
# https://www.reddit.com/r/NixOS/comments/s9ytrg/comment/htr06n8/

let
  cfg = config.myconfig;
  enable = cfg.wayland.enable && config.services.dbus.enable;

  # currently, there is some friction between sway and gtk:
  # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
  # the suggested way to set gtk settings is with gsettings
  # for gsettings to work, we need to tell it where the schemas are
  # using the XDG_DATA_DIR environment variable
  # run at the end of sway config
  configure-gtk = let
    schema = pkgs.gsettings-desktop-schemas;
    datadir = "${schema}/share/gsettings-schemas/${schema.name}";
  in pkgs.writeShellScriptBin "configure-gtk" ''
    export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
    gnome_schema=org.gnome.desktop.interface
    ${pkgs.glib}/bin/gsettings set $gnome_schema gtk-theme 'Dracula'
  '';

  # bash script to let dbus know about important env variables and
  # propogate them to relevent services run at the end of sway config
  # see
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/"It-doesn't-work"-Troubleshooting-Checklist
  # note: this is pretty much the same as  /etc/sway/config.d/nixos.conf but also restarts
  # some user services to make sure they have the correct environment variables
  dbus-wm-environment = pkgs.writeShellScriptBin "dbus-wm-environment" ''
    dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=''${XDG_CURRENT_DESKTOP:-sway}
    systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
    systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
  '';

in {
  config = lib.mkIf enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        configure-gtk
        dbus-wm-environment
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
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      # gtk portal needed to make gtk apps happy
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      # warning: The option `xdg.portal.gtkUsePortal'has been deprecated. Setting the variable globally with `environment.sessionVariables' NixOS option can have unforseen side-effects.
      gtkUsePortal = true;
    };
  } // (lib.mkIf (! enable) {
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [
          (writeShellScriptBin "configure-gtk" ''
            echo "nothing to do, as config.service.dbus.enable=false"
          '')
          (writeShellScriptBin "dbus-wm-environment" ''
            echo "nothing to do, as config.service.dbus.enable=false"
          '')
        ];
    }];
  });
}