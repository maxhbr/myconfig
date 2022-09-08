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
    ];
    extraSessionCommands = ''
      export XKB_DEFAULT_LAYOUT=de
      export XKB_DEFAULT_VARIANT=neo
    '';
    withGtkWrapper = true;
    extraOptions = [ ];
  };
in {
  options.myconfig = with lib; { river.enable = mkEnableOption "river"; };
  config = (lib.mkIf cfg.river.enable {
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
