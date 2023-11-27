{ config, lib, pkgs, ... }:

# https://nixos.wiki/wiki/Sway
# https://www.reddit.com/r/NixOS/comments/s9ytrg/comment/htr06n8/

let
  # bash script to let dbus know about important env variables and
  # propogate them to relevent services run at the end of sway config
  # see
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/"It-doesn't-work"-Troubleshooting-Checklist
  # note: this is pretty much the same as  /etc/sway/config.d/nixos.conf but also restarts
  # some user services to make sure they have the correct environment variables
  dbus-wm-environment = pkgs.writeShellScriptBin "dbus-wm-environment" ''
    dbus-update() (
      set -x
      export XDG_CURRENT_DESKTOP="''${1:-wlroots}"
      dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP=$XDG_CURRENT_DESKTOP
      if command -v configure-gtk &> /dev/null; then
          configure-gtk &disown
      fi
    )
    systemctl-update() (
      set -x
      systemctl --user stop pipewire ${
        if config.services.pipewire.wireplumber.enable then
          "wireplumber"
        else
          "pipewire-media-session"
      } xdg-desktop-portal xdg-desktop-portal-wlr
      systemctl --user start pipewire ${
        if config.services.pipewire.wireplumber.enable then
          "wireplumber"
        else
          "pipewire-media-session"
      } xdg-desktop-portal xdg-desktop-portal-wlr
    )

    if [[ $# -eq 0 ]] ; then
      exec "$0" "''${XDG_CURRENT_DESKTOP:-wlroots}"
    elif [[ "$1" == "systemctl-update" ]]; then
      shift
      echo "systemctl-update..."
      systemctl-update "$@"
    else
      echo "dbus-update..."
      dbus-update "$@"
      until "$0" systemctl-update $@; do 
        echo "wait for detached..."
        wait $(jobs -p) || true
        echo "retry..."
      done
    fi
  '';

in {
  config = lib.mkIf config.services.dbus.enable {
    home-manager.sharedModules =
      [{ home.packages = with pkgs; [ dbus-wm-environment ]; }];

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

      # trace: warning: xdg-desktop-portal 1.17 reworked how portal implementations are loaded, you
      # should either set `xdg.portal.config` or `xdg.portal.configPackages`
      # to specify which portal backend to use for the requested interface.
      #
      # https://github.com/flatpak/xdg-desktop-portal/blob/1.18.1/doc/portals.conf.rst.in
      #
      # If you simply want to keep the behaviour in < 1.17, which uses the first
      # portal implementation found in lexicographical order, use the following:
      #
      # xdg.portal.config.common.default = "*";
      config = { common = { default = "wlr"; }; };
      wlr.enable = true;
      wlr.settings.screencast =
        lib.mkIf config.myconfig.desktop.wayland.enable {
          output_name = "eDP-1";
          chooser_type = "simple";
          chooser_cmd = "${pkgs.slurp}/bin/slurp -f %o -or";
        };
      # # gtk portal needed to make gtk apps happy
      # extraPortals =
      #   let gnome = config.services.xserver.desktopManager.gnome.enable;
      #   in [ pkgs.xdg-desktop-portal-wlr ]
      #   ++ lib.optional (!gnome) pkgs.xdg-desktop-portal-gtk;
    };
  };
}
