# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  # bash script to let dbus know about important env variables and
  # propogate them to relevent services run at the end of sway config
  # see
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/"It-doesn't-work"-Troubleshooting-Checklist
  # note: this is pretty much the same as  /etc/sway/config.d/nixos.conf but also restarts
  # some user services to make sure they have the correct environment variables
  dbus-sway-environment = pkgs.writeTextFile {
    name = "dbus-sway-environment";
    destination = "/bin/dbus-sway-environment";
    executable = true;

    text = ''
      dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
      systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
      systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
    '';
  };

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
      gsettings set $gnome_schema gtk-theme 'Dracula'
    '';
  };

  cfg = config.myconfig;
in {
  config = (lib.mkIf (cfg.wayland.enable && config.programs.sway.enable) {
    environment = {
      systemPackages = with pkgs; [
        dbus-sway-environment
        configure-gtk
        dracula-theme # gtk theme
        gnome3.adwaita-icon-theme # default gnome cursors
      ];
      etc."sway/config".source = ./sway/config;
    };
    home-manager.sharedModules = [{
      home.file = { ".config/sway/config".source = ./sway/config; };
      home.packages = with pkgs; [
        qt5.qtwayland
      ];
        programs.waybar.enable = true;
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
      gtkUsePortal = true;
    };

    programs.sway = {
      extraPackages = with pkgs; [
        autotiling
        swaylock
        swayidle
        xwayland
        st
        dmenu
      ] ++ cfg.wayland.commonPackages;
      wrapperFeatures.gtk = true;

      extraSessionCommands = ''
        export XDG_CURRENT_DESKTOP=sway
        export XKB_DEFAULT_LAYOUT=${config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"}
        export XKB_DEFAULT_VARIANT=${config.environment.sessionVariables."XKB_DEFAULT_VARIANT"}
        export XDG_SESSION_TYPE=${config.environment.sessionVariables."XDG_SESSION_TYPE"}
        export SDL_VIDEODRIVER=${config.environment.sessionVariables."SDL_VIDEODRIVER"}
        export QT_QPA_PLATFORM=${config.environment.sessionVariables."QT_QPA_PLATFORM"}
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=${config.environment.sessionVariables."QT_WAYLAND_DISABLE_WINDOWDECORATION"}
        export _JAVA_AWT_WM_NONREPARENTING=${config.environment.sessionVariables."_JAVA_AWT_WM_NONREPARENTING"}
      '';
    };

    myconfig.wayland.greetdSettings = {
      sway_session = {
        command = "sway";
        user = "mhuber";
      };
    };
  });
}
