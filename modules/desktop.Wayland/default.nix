# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  settingsFormat = pkgs.formats.toml { };
in {
  options.myconfig = with lib; {
    wayland = {
      enable = mkEnableOption "wayland";
      commonPackages = mkOption {
        type = with types; listOf package;
        default = with pkgs; [
          xwayland
          ## Terminal
          foot
          (writeShellScriptBin "tfoot" ''
            exec ${foot}/bin/foot ${tmux}/bin/tmux
          '')
          (writeShellScriptBin "tfoot-reattach" ''
            ${tmux}/bin/tmux ls |
                ${gnugrep}/bin/grep -v '(attached)' |
                cut -f 1 -d ":" |
                while read SESSION; do
                    (set -x;
                    ${foot}/bin/foot ${tmux}/bin/tmux attach -t "$SESSION" & disown)
                done
          '')
          # https://github.com/riverwm/river/wiki/Recommended-Software
          ## Output configuration
          wlopm
          way-displays # wlr-randr kanshi
          ## statusbar
          waybar
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
            exec ${swayidle}/bin/swayidle -w \
            	timeout 300 '${swaylock}/bin/swaylock -f -c 000000' \
            	before-sleep '${swaylock}/bin/swaylock -f -c 000000'
          '')
          # (writeShellScriptBin "myphyslock"
          #   "exec '${config.security.wrapperDir}/physlock'")
          ## Other
          swaybg
          wayshot
          wf-recorder
          slurp
          grim
          (writeShellScriptBin "grim-region" ''
            output_dir="$HOME/_screenshots"
            old_dir="$output_dir/_old"
            mkdir -p "$output_dir"
            mkdir -p "$old_dir"

            if [[ "$1" == "win" ]]; then
                shift
                output="$output_dir/$(date +%Y-%m-%d_%H-%M-%S).png"
            else
                output="$output_dir/$(date +%Y-%m-%d_%H:%M:%S).png"
            fi

            echo "## clean up old screenshots ..."
            find "$output_dir" -maxdepth 1 -mtime +10 -type f -print -exec mv {} "$old_dir" \;

            ${grim}/bin/grim \
              -o "$output" \
              -g "$(${slurp}/bin/slurp)"
          '')
          wob
          wl-clipboard
          # xdg-desktop-portal-wlr
          nomacs
          dex # for autostarting
        ];
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
      desktop = mkOption {
        type = types.str;
        default = optionalString cfg.wayland.enable "river";
        defaultText = literalExpression ''
          optionalString config.myconfig.wayland.enable "river"
        '';
        description = lib.mdDoc ''
          The desktop environment to use
        '';
      };
      greetdSettings = mkOption {
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

  imports = [ ./services.dbus.nix ];
  config = (lib.mkIf cfg.wayland.enable {
    environment.sessionVariables = {
      "XDG_SESSION_TYPE" = "wayland";
      "SDL_VIDEODRIVER" = "wayland";
      # needs qt5.qtwayland in systemPackages
      "QT_QPA_PLATFORM" = "wayland";
      "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
    };
    services.greetd = {
      enable = true;
      settings = let
        chosen_session =
          cfg.wayland.greetdSettings."${cfg.wayland.desktop}_session";
      in cfg.wayland.greetdSettings // {
        default_session = {
          command = "${
              lib.makeBinPath [ pkgs.greetd.tuigreet ]
            }/tuigreet --width 120 --time --cmd '${chosen_session.command}'";
          user = "greeter";
        };
        # initial_session = chosen_session;
      };
    };
    home-manager.sharedModules = [
      ./home-manager.kanshi.nix
      ./home-manager.mako.nix
      ./home-manager.waybar.nix
      {
        home.packages = with pkgs; [
          (writeShellScriptBin "regreet"
            "sudo systemctl restart greetd.service")
          qt5.qtwayland
        ];
        xdg.configFile = {
          "way-displays/cfg.yaml".source = ./way-displays/cfg.yaml;
        };
        programs.waybar.enable = true;
        services.kanshi.enable = true;
        programs.mako.enable = true;
        services.random-background.enable = lib.mkForce false;
        services.screen-locker.enable = lib.mkForce false;
      }
    ];
    services.physlock = {
      enable = lib.mkForce false;
      allowAnyUser = true;
    };
  });
}
