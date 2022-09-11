# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    wayland = {
      enable = mkEnableOption "wayland";
      commonPackages = mkOption {
        type = with types; listOf package;
        default = with pkgs; [
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
          ## Other
          swaybg
          wayshot
          wf-recorder
          slurp
          grim
          (writeShellScriptBin "grim-region" "${grim}/bin/grim -g \"$(${slurp}/bin/slurp)\"")
          wl-clipboard
          xdg-desktop-portal-wlr
          nomacs
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
        default = optionalString cfg.wayland.enable
          "river";
        defaultText = literalExpression ''
          optionalString config.myconfig.wayland.enable "river"
        '';
        description = lib.mdDoc ''
          The desktop environment to use
        '';
      };
    };
  };
  config = (lib.mkIf cfg.wayland.enable {
    environment.sessionVariables = {
      "XDG_CURRENT_DESKTOP" = cfg.wayland.desktop;
      "XDG_SESSION_TYPE" = "wayland";
      "SDL_VIDEODRIVER" = "wayland";
      # needs qt5.qtwayland in systemPackages
      "QT_QPA_PLATFORM" = "wayland";
      "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
    };
    home-manager.sharedModules = [
      {
        services.random-background.enable = lib.mkForce false;
        programs.mako.enable = true;
      }
    ];
    services.greetd = {
      enable = true;
    };
    services.physlock = {
      enable = true;
      allowAnyUser = true;
    };
  });
}
