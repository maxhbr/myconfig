# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  riverPackage = pkgs.callPackage ./wrapper.nix {
    river-unwrapped = pkgs.river;
    withBaseWrapper = true;
    extraPaths = with pkgs; [
      (writeShellScriptBin "reriver" "sudo systemctl restart greetd.service")
      rivercarro
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
      bemenu
      fuzzel
      ## Screen Lockers
      swaylock
      ## Idle Management
      swayidle
      (writeShellScriptBin "myswayidle" ''
        set -euo pipefail
        ${swayidle}/bin/swayidle -w \
          timeout 300 '${config.security.wrapperDir}/physlock' \
          before-sleep '${config.security.wrapperDir}/physlock'
      '')
      (writeShellScriptBin "myphyslock"
        "exec '${config.security.wrapperDir}/physlock'")
      ## Other
      swaybg
      ristate
      wayshot
      wf-recorder
      slurp
      grim
      (writeShellScriptBin "grim-region" "${grim}/bin/grim -g \"$(${slurp}/bin/slurp)\"")
      wl-clipboard
      xdg-desktop-portal-wlr
    ];
    extraSessionCommands = ''
      export XKB_DEFAULT_LAYOUT=${config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"}
      export XKB_DEFAULT_VARIANT=${config.environment.sessionVariables."XKB_DEFAULT_VARIANT"}
      export XDG_SESSION_TYPE=${config.environment.sessionVariables."XDG_SESSION_TYPE"}
      export SDL_VIDEODRIVER=${config.environment.sessionVariables."SDL_VIDEODRIVER"}
      export QT_QPA_PLATFORM=${config.environment.sessionVariables."QT_QPA_PLATFORM"}
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=${config.environment.sessionVariables."QT_WAYLAND_DISABLE_WINDOWDECORATION"}
      export _JAVA_AWT_WM_NONREPARENTING=${config.environment.sessionVariables."_JAVA_AWT_WM_NONREPARENTING"}
    '';
    withGtkWrapper = true;
    extraOptions = [ ];
  };
in {
  options.myconfig = with lib; { river.enable = mkEnableOption "river"; };
  config = (lib.mkIf cfg.river.enable {
    environment.sessionVariables = {
      "XDG_CURRENT_DESKTOP" = "river";
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
        xdg.configFile = {
          "river/init".source = ./river/init;
          "way-displays/cfg.yaml".source = ./way-displays/cfg.yaml;
        };
        home.packages = with pkgs; [ riverPackage ];
        services.random-background.enable = lib.mkForce false;
      }
      { programs.mako.enable = true; }
      ./home-manager.waybar.nix
      {programs.waybar.enable = true; }
    ];
    services = {
      xserver.displayManager.sddm = {
        settings = { General.DefaultSession = "river.desktop"; };
      };
    };
    services.xserver.displayManager.sessionPackages = [ riverPackage ];
    services.greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          command = "${riverPackage}/bin/river";
          user = "mhuber";
        };
        default_session = initial_session;
      };
    };
    services.physlock = {
      enable = true;
      allowAnyUser = true;
    };
  });
}
