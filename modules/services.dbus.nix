{ config, lib, pkgs, ... }:

# https://nixos.wiki/wiki/Sway
# https://www.reddit.com/r/NixOS/comments/s9ytrg/comment/htr06n8/

let
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
    dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP=''${XDG_CURRENT_DESKTOP:-sway}

    ${configure-gtk}/bin/configure-gtk &disown

    systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
    systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
  '';

in {
  config = lib.mkIf config.services.dbus.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        configure-gtk
        dbus-wm-environment
        dracula-theme # gtk theme
        gnome3.adwaita-icon-theme # default gnome cursors
      ];
    }];

    environment.etc = lib.mkIf config.programs.sway.enable {
      # overwrite the nixos.conf from https://github.com/NixOS/nixpkgs/blob/4ae405c83424f18b360dc9794f6300ab243f61e2/nixos/modules/programs/sway.nix#L129-L133
      "sway/config.d/nixos.conf" = lib.mkForce {
        source = pkgs.writeText "nixos.conf" ''
          exec ${dbus-wm-environment}/bin/dbus-wm-environment sway
        '';
      };
    };

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
      extraPortals = let
        gnome = config.services.xserver.desktopManager.gnome.enable;
      in [ pkgs.xdg-desktop-portal-wlr ] ++ lib.optional (!gnome) pkgs.xdg-desktop-portal-gtk;
      # # warning: The option `xdg.portal.gtkUsePortal'has been deprecated. Setting the variable globally with `environment.sessionVariables' NixOS option can have unforseen side-effects.
      # gtkUsePortal = true;
    };
  };
}
