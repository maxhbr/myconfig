# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
  debug = false;
  hyprlandPkg = pkgs.hyprland.override {
    inherit debug;
    legacyRenderer = cfg.desktop.wayland.hyprland.legacyRenderer;
  };
  hyprnomePkg = pkgs.hyprnome.override (old: {
    rustPlatform = old.rustPlatform // {
      buildRustPackage = args:
        old.rustPlatform.buildRustPackage (args // {
          version =
            "2024-05-07"; # currentl 0.2.0 is the latest released version
          src = pkgs.fetchFromGitHub {
            owner = "donovanglover";
            repo = "hyprnome";
            rev = "f185e6dbd7cfcb3ecc11471fab7d2be374bd5b28";
            hash = "sha256-tmko/bnGdYOMTIGljJ6T8d76NPLkHAfae6P6G2Aa2Qo=";
          };
          cargoHash = "sha256-PKmOsep0YS5LHnrH4wlgeey7X/9szgGsp1uSbRI5sj4=";
        });
    };
  });
in {
  options.myconfig = with lib; {
    desktop.wayland.hyprland = {
      legacyRenderer = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };
  imports = [{ programs.hyprland = { enable = false; }; }];
  config = (lib.mkIf (cfg.desktop.wayland.enable
    && builtins.elem "hyprland" cfg.desktop.wayland.selectedSessions) {
      home-manager.sharedModules = [
        ({ config, ... }:
          let
            hyprctl =
              "${config.wayland.windowManager.hyprland.package}/bin/hyprctl";
            hyprctl-scripts = with pkgs; [
              (writeShellScriptBin "hyprctl-animations-off" ''
                ${hyprctl} keyword animations:enabled false
                ${hyprctl} keyword decoration:blur:enabled false
                ${hyprctl} keyword decoration:inactive_opacity 1
                ${hyprctl} keyword decoration:dim_inactive false
                ${hyprctl} keyword decoration:dim_special 1
              '')
              (writeShellScriptBin "hyprctl-animations-on"
                "${hyprctl} keyword animations:enabled true")
              (writeShellScriptBin "hyprctl-create-headless" ''
                ${hyprctl} output create headless
                echo "remove with:"
                echo "  hyprctl output remove HEADLESS-2"
              '')
            ];
          in {
            home.sessionVariables =
              lib.mkIf debug { ASAN_OPTIONS = "log_path=/tmp/asan.log"; };
            home.packages = with pkgs;
              [ hyprpaper hyprnomePkg hyprpicker ] ++ hyprctl-scripts
              ++ (if debug then [ kitty ] else [ ]);
            wayland.windowManager.hyprland = {
              enable = true;
              package = hyprlandPkg;
              extraConfig = ''
                $notifycmd = ${pkgs.libnotify}/bin/notify-send -h string:x-canonical-private-synchronous:hypr-cfg -u low
                source = ${./hypr}/hyprland.conf

                exec-once = ${
                  pkgs.writeShellScriptBin "autostart.sh" ''
                    set -x
                    exec &> >(tee -a /tmp/hyprland.''${XDG_VTNR}.''${USER}.autostart.log)
                    ${cfg.desktop.wayland.autostartCommands}
                    waybarOnce hyprland &disown
                  ''
                }/bin/autostart.sh
              '';
            };
            programs.waybar = {
              enable = lib.mkDefault true;
              settings.mainBar = {
                modules-left = [ "hyprland/workspaces" "hyprland/submap" ];
                "hyprland/workspaces" = {
                  "format" = "{icon}";
                  "format-icons" = {
                    "1" = "u<sub> 1 </sub>";
                    "2" = "i<sub> 2 </sub>";
                    "3" = "a<sub> 3 </sub>";
                    "4" = "e<sub> 4 </sub>";
                    "5" = "o<sub> 5 </sub>";
                    "6" = "s<sub> 6 </sub>";
                    "7" = "n<sub> 7 </sub>";
                    "8" = "r<sub> 8 </sub>";
                    "9" = "t<sub> 9 </sub>";
                    "10" = "d<sub> 0 </sub>";
                  };
                  # "persistent-workspaces" = {
                  #   "*" = [ 1 2 3 4 5 6 7 8 9 10 ];
                  # };
                  "on-scroll-up" = "${hyprnomePkg}/bin/hyprnome";
                  "on-scroll-down" = "${hyprnomePkg}/bin/hyprnome --previous";
                  "show-special" = true;
                };
                "hyprland/window" = {
                  "max-length" = 200;
                  "rewrite" = {
                    "(.*) — Mozilla Firefox" = "FF: $1";
                    "(.*) - Visual Studio Code" = "code: $1";
                    "(.*) - fish" = "> [$1]";
                  };
                  "separate-outputs" = true;
                };
                "hyprland/submap" = {
                  "format" = "submap: {}";
                  "max-length" = 8;
                  "tooltip" = false;
                  "rotate" = 90;
                };
              };
            };
          })
      ];
      myconfig.desktop.wayland.sessions = {
        hyprland = { command = "${hyprlandPkg}/bin/Hyprland"; };
      };
    });
}
