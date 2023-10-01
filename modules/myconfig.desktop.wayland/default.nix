# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
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

              echo "## take screenshot ..."
              GRIM_DEFAULT_DIR="$output_dir" ${grim}/bin/grim \
                -g "$(${slurp}/bin/slurp)" \
                "$output_dir/$(date +%Y-%m-%d_%H-%M-%S).png"
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
          ] ++ [ wayvnc ];
        # defaultText = literalExpression ''
        #   with pkgs; [ ];
        # '';
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
        '';
        # ${pkgs.dex}/bin/dex --autostart
      };

      desktop = mkOption {
        type = types.str;
        default = optionalString cfg.desktop.wayland.enable "river";
        defaultText = literalExpression ''
          optionalString config.myconfig.wayland.enable "river"
        '';
        description = lib.mdDoc ''
          The desktop environment to use
        '';
      };
      greetdSettings = let settingsFormat = pkgs.formats.toml { };
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
    (lib.mkIf (cfg.desktop.wayland.enable && config.services.greetd.enable) {
      services.greetd = {
        settings = let
          chosen_session =
            cfg.desktop.wayland.greetdSettings."${cfg.desktop.wayland.desktop}_session";
        in cfg.desktop.wayland.greetdSettings // {
          default_session = {
            command = "${
                lib.makeBinPath [ pkgs.greetd.tuigreet ]
              }/tuigreet --width 120 --time --cmd '${chosen_session.command}'";
            user = "greeter";
          };
          # initial_session = chosen_session;
        };
      };
      home-manager.sharedModules = [{
        home.packages = with pkgs;
          [
            (writeShellScriptBin "regreet"
              "sudo systemctl restart greetd.service")
          ];
      }];
    })
    (lib.mkIf (cfg.desktop.wayland.enable && !config.services.greetd.enable) {
      environment.interactiveShellInit = let
        chosen_session =
          cfg.desktop.wayland.greetdSettings."${cfg.desktop.wayland.desktop}_session";
      in ''
        if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
          exec &> >(tee /tmp/tty1-wayland.''${XDG_VTNR}.''${USER}.log)
          exec ${chosen_session.command} > /tmp/tty1-wayland.''${XDG_VTNR}.''${USER}.log
        fi
      '';
    })
  ];

  config = (lib.mkIf cfg.desktop.wayland.enable {
    environment.sessionVariables = {
      "NIXOS_OZONE_WL" = "1";
      "XDG_SESSION_TYPE" = "wayland";
      "SDL_VIDEODRIVER" = "wayland";
      # needs qt5.qtwayland in systemPackages
      "QT_QPA_PLATFORM" = "wayland";
      "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
    };

    services.greetd.enable = true;
    # services.xserver.displayManager.sddm.wayland = true;
    # services.xserver.displayManager.gdm.wayland = true;

    home-manager.sharedModules = [
      ./home-manager.kanshi.nix
      ./home-manager.mako.nix
      ./home-manager.waybar
      ./home-manager.foot.nix
      {
        home.packages = cfg.desktop.wayland.commonPackages;
        xdg.configFile = {
          "way-displays/cfg.yaml".source = ./way-displays/cfg.yaml;
        };
        programs.waybar.enable = true;
        services.kanshi.enable = false;
        services.mako.enable = true;
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
      # {
      #   programs.foot = let
      #     foottheme = "";

      #     # imaterial-amber = {
      #     #   cursor = {
      #     #     color = "fff8e1 21201d";
      #     #   };
      #     #   colors = {
      #     #     foreground = "21201d";
      #     #     background = "fff8e1";

      #     #     regular0 = "21201d"; # black
      #     #     regular1 = "cd4340"; # red
      #     #     regular2 = "498d49"; # green
      #     #     regular3 = "fab32d"; # yellow
      #     #     regular4 = "3378c4"; # blue
      #     #     regular5 = "b83269"; # magenta
      #     #     regular6 = "21929a"; # cyan
      #     #     regular7 = "ffd7d7"; # white

      #     #     bright0 = "66635a"; # bright black
      #     #     bright1 = "dd7b72"; # bright red
      #     #     bright2 = "82ae78"; # bright green
      #     #     bright3 = "fbc870"; # bright yellow
      #     #     bright4 = "73a0cd"; # bright blue
      #     #     bright5 = "ce6f8e"; # bright magenta
      #     #     bright6 = "548c94"; # bright cyan
      #     #     bright7 = "ffe1da"; # bright white

      #     #     dim0 = "9e9a8c"; # dim black
      #     #     dim1 = "e9a99b"; # dim red
      #     #     dim2 = "b0c99f"; # dim green
      #     #     dim3 = "fdda9a"; # dim yellow
      #     #     dim4 = "a6c0d4"; # dim blue
      #     #     dim5 = "e0a1ad"; # dim magenta
      #     #     dim6 = "3c6064"; # dim cyan
      #     #     dim7 = "ffe9dd"; # dim white
      #     #   };
      #     # };
      #   in {
      #     enable = true;
      #     server.enable = true;
      #     settings =  {
      #       main = {
      #         term = "xterm-256color";

      #         font = "font=monospace:size=8";
      #         dpi-aware = "yes";
      #         include = "${foottheme}";
      #       };
      #       colors = {
      #         alpha = "0.8";
      #       };
      #     } // imaterial-amber;
      #   };
      # }
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
