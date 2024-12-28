# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.wayland = {
      commonPackages = mkOption {
        type = with types; listOf package;
        default = with pkgs;
          [
            # xwayland
            xorg.xwininfo # to find out if somenthing runs under xwayland
            ## Terminal
            # https://github.com/riverwm/river/wiki/Recommended-Software
            ## Output configuration
            wlopm
            way-displays
            wdisplays
            wlr-randr
            cage # allows to run x windows
            ## Program Launchers
            wofi
            bemenu
            fuzzel
            ## Screen Lockers
            swaylock
            ## Idle Management
            swayidle
            (writeShellScriptBin "myswayidle" ''
              set -x
              color=777777
              exec ${swayidle}/bin/swayidle -w \
                  timeout 300 '${swaylock}/bin/swaylock -f -c '"$color" \
                  before-sleep '${swaylock}/bin/swaylock -f -c '"$color"
            '')
            ## Other
            wayshot
            wf-recorder
            slurp
            grim
            (writeShellScriptBin "grim-region" ''
              output_dir="$HOME/_screenshots"
              old_dir="$output_dir/_old"
              mkdir -p "$output_dir"
              mkdir -p "$old_dir"

              echo "## clean up old screenshots ..."
              find "$output_dir" -maxdepth 1 -mtime +10 -type f -print -exec mv {} "$old_dir" \;

              out="$output_dir/$(date +%Y-%m-%d_%H-%M-%S).png"
              echo "## take screenshot: $out ..."
              GRIM_DEFAULT_DIR="$output_dir" ${grim}/bin/grim \
                -g "$(${slurp}/bin/slurp)" \
                "$out"
            '')
            (writeShellScriptBin "x_to_w" ''
              set -euo pipefail
              x_to_w_inner() {
                local x_arg="''${1:-}"
                local wl_arg="''${2:-}"
                x11_clip=$(${xclip}/bin/xclip -selection "$x_arg" -o 2>/dev/null)
                if [ $? -eq 0 ] && [ -n "$x11_clip" ]; then
                    # TODO: Handle types
                    echo -n "$x11_clip" | ${wl-clipboard}/bin/wl-copy "$wl_arg" 
                fi
              }
              x_to_w_inner "clipboard" ""
              x_to_w_inner "primary" "--primary"
            '')
            (writeShellScriptBin "w_to_x" ''
              set -euo pipefail
              w_to_x_inner() {
                local wl_arg="''${1:-}"
                local x_arg="''${2:-}"
                wayland_clip="$(${wl-clipboard}/bin/wl-paste $wl_arg 2>/dev/null)"
                if [ $? -eq 0 ] && [ -n "$wayland_clip" ]; then
                    wayland_type="$(wl-paste -l | head -1 | tr -d '\n')"
                    if [ -n "$wayland_type" ]; then
                        echo -n "$wayland_clip" | ${xclip}/bin/xclip -i -t "$wayland_type" -selection $x_arg
                    else
                        echo -n "$wayland_clip" | ${xclip}/bin/xclip -i -selection $x_arg
                    fi
                fi
                
              }
              w_to_x_inner "" "clipboard"
              w_to_x_inner "--primary" "primary"
            '')
            wob # A lightweight overlay bar for Wayland
            wl-clipboard
            lswt # list Wayland toplevels
            # xdg-desktop-portal-wlr
            nomacs
            dex # for autostarting
            gammastep
            wev # Wayland event viewer
            qt5.qtwayland
          ] ++ [ wayvnc waypipe ];
        example = literalExpression ''
          with pkgs; [ ]
        '';
        description = lib.mdDoc ''
          Extra packages to be installed in the desktop environment wide.
        '';
      };
      autostartCommands = mkOption {
        type = types.lines;
        default = with pkgs; ''
          myswayidle 3600 &
          ${playerctl}/bin/playerctld daemon &
          ${pkgs.swaybg}/bin/swaybg \
              -c '#556677' \
              -i '${pkgs.my-wallpapers}/share/romben3.png' \
              >/dev/null 2>&1 &

          ${foot}/bin/foot --server &
          {
            tfoot || ${pkgs.foot}/bin/foot
          } >/dev/null 2>&1 &

          ${wlsunset}/bin/wlsunset &

          ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
        '';
        # ${pkgs.dex}/bin/dex --autostart
      };

      selectedSessions = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = lib.mdDoc ''
          List of greet desktop environments, fist will be default
        '';
      };
      selectedGreeter = mkOption {
        type = types.enum [ "tuigreet" "gtkgreet" ];
        default = "tuigreet";
        description = lib.mdDoc ''
          Choose the greeter to use
        '';
      };
      directLoginFirstSession = mkOption {
        type = types.bool;
        default = false;
      };
      sessions = let settingsFormat = pkgs.formats.toml { };
      in mkOption {
        type = settingsFormat.type;
        example = literalExpression ''
          {
            sway = {
              command = "''${pkgs.greetd.greetd}/bin/agreety --cmd sway";
            };
          }
        '';
        description = lib.mdDoc ''
          greetd configuration ([documentation](https://man.sr.ht/~kennylevinsen/greetd/))
          as a Nix attribute set.
        '';
      };
    };
  };

  imports = [ 
    ./services.greetd.nix
    ./sharescreen.nix
    ./programs.waybar
    {
      home-manager.sharedModules = [
        {
          options.myconfig = with lib; {
            desktop.wayland = {
              enable = mkEnableOption "wayland";
            };
          };
        }
        ./home-manager.wrap-electron-apps.nix
      ];
    }
  ];

  config = (lib.mkIf cfg.desktop.wayland.enable {
    services.greetd.enable = true;
    environment.sessionVariables = {
      # "NIXOS_OZONE_WL" = "1"; # VSCode fails to start if that is set in dwl / wayland
      "XDG_SESSION_TYPE" = "wayland";
      "SDL_VIDEODRIVER" = "wayland";
      # needs qt5.qtwayland in systemPackages
      "QT_QPA_PLATFORM" = "wayland";
      "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
    };

    # services.xserver.displayManager.sddm.wayland = true;
    # services.xserver.displayManager.gdm.wayland = true;
    services.libinput.enable = true;

    nixpkgs = {
      overlays = [
        # Disable some things that don’t cross compile
        # from https://github.com/matthewbauer/nixiosk/blob/7e6d1e1875ec5ae810e99fe5a1c814abdf56fecb/configuration.nix#L104
        (self: super:
          lib.optionalAttrs
          (super.stdenv.hostPlatform != super.stdenv.buildPlatform) {
            gtk3 = super.gtk3.override { cupsSupport = false; };
            mesa = super.mesa.override { eglPlatforms = [ "wayland" ]; };
          })
      ];
    };

    home-manager.sharedModules = [
      {
        myconfig.desktop.wayland.enable = true;
      }
      ./home-manager.kanshi.nix
      ./home-manager.swaync.nix
      ./home-manager.mako.nix
      ./home-manager.foot.nix
      {
        home.packages = cfg.desktop.wayland.commonPackages;
        xdg.configFile = {
          "way-displays/cfg.yaml".source = ./way-displays/cfg.yaml;
        };
        programs.waybar.enable = true;
        services.kanshi.enable = false;
        services.swaync.enable = true;
        services.mako.enable = false;
        services.dunst.enable = false;
        services.random-background.enable = lib.mkForce false;
        services.screen-locker.enable = lib.mkForce false;
        programs.foot.enable = true;
      }
      {
        programs.wofi = {
          enable = true;
          settings = {
            mode = "run";
            location = "bottom-right";
            allow_markup = true;
            width = 250;
          };
        };
      }
    ];
    services.physlock.enable = lib.mkForce false;

    # https://github.com/NixOS/nixpkgs/issues/143365
    # security.pam.services.swaylock = {};
    security.pam.services.swaylock.text = ''
      # Account management.
      account required pam_unix.so

      # Authentication management.
      auth sufficient pam_unix.so   likeauth try_first_pass
      auth required pam_deny.so

      # Password management.
      password sufficient pam_unix.so nullok sha512

      # Session management.
      session required pam_env.so conffile=/etc/pam/environment readenv=0
      session required pam_unix.so
    '';

  });
}
