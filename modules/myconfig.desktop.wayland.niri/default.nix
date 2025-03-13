# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, inputs, ... }:
let
  nixosConfig = config;
  cfg = config.myconfig;
  user = myconfig.user;
  niri = pkgs.niri;
in {
  # add option for additional config added to config.kdl
  options.myconfig = with lib; {
    desktop.wayland.niri.additionalConfigKdl = lib.mkOption {
      type = types.str;
      default = "";
      description = "Additional config added to config.kdl";
    };
  };
  config = (lib.mkIf (cfg.desktop.wayland.enable
    && (builtins.elem "niri" cfg.desktop.wayland.selectedSessions
      || builtins.elem "niri-plain" cfg.desktop.wayland.selectedSessions)) {
        nixpkgs.overlays = [ 
          # do not use provided overlay to reduce risk
          (_: _: {
            niri = inputs.niri.packages.${pkgs.system}.default;
          })
        ];
        home-manager.sharedModules = [
          ({ config, ... }: let
            niri-autostart = pkgs.writeShellScript "niri-autostart" ''
              set -x
              exec &> >(tee -a /tmp/niri.''${XDG_VTNR}.''${USER}.autostart.log)
              ${cfg.desktop.wayland.autostartCommands}
            '';
            niri-xwayland-satellite = pkgs.writeShellScriptBin "niri-xwayland-satellite" ''
              CHOSEN_DISPLAY="''${1:-"''${DISPLAY}"}"
              pidfile=/tmp/niri.''${XDG_VTNR}.''${USER}.xwayland-satellite.''${CHOSEN_DISPLAY}.pid
              exec &> >(tee -a /tmp/niri.''${XDG_VTNR}.''${USER}.xwayland-satellite.''${CHOSEN_DISPLAY}.log)
            
              if [ -f $pidfile ]; then
                kill $(cat $pidfile) || true
              fi
            
              set -euo pipefail
              set -x
              ${pkgs.xwayland-satellite}/bin/xwayland-satellite ''${CHOSEN_DISPLAY}
              echo $$ > $pidfile
            '';
          in {
            home.sessionVariables = {
              DISPLAY = ":12";
            };
            home.packages = [ niri niri-xwayland-satellite ];
            xdg.configFile = {
              "niri/config.kdl".source = let
                drv = pkgs.runCommand "niri-config" {
                  nativeBuildInputs = [ niri ];
                  src = ./config.kdl;
                } ''
                  mkdir $out
                  cat <<EOF >$out/config.kdl
                  $(cat $src)

                  environment {
                      DISPLAY "${config.home.sessionVariables.DISPLAY}"
                  }

                  ${cfg.desktop.wayland.niri.additionalConfigKdl}

                  spawn-at-startup "${niri-autostart}"
                  spawn-at-startup "${niri-xwayland-satellite}/bin/niri-xwayland-satellite"
                  EOF
                  niri validate --config $out/config.kdl > $out/config.kdl.validate
                '';
              in "${drv}/config.kdl";
            };
            programs.waybar.settings.mainBar = {
              "niri/window" = {
                "format" =  "{}";
                "rewrite" = {
                  "(.*) - Mozilla Firefox" = "🌎 $1";
                  "(.*) - zsh" = "> [$1]";
                };
                rotate = 90;
              };
              "niri/workspaces" = {
                "format" = "{icon}";
                "format-icons" = {
                  # Named workspaces
                  # (you need to configure them in niri)
                  "browser" = "";
                  "discord" = "";
                  "chat" = "<b></b>";

                  # Icons by state
                  "active" = "";
                  "default" = "";
                };
                rotate = 90;
              };
              modules-left = [
                # "niri/workspaces"
                "wlr/taskbar"
              ];
              modules-center = [
                # "niri/window"
              ];
            };
            systemd.user.services.niri = {
              Unit = {
                Description = "A scrollable-tiling Wayland compositor";
                BindsTo = "graphical-session.target";
                Before =
                  "graphical-session.target xdg-desktop-autostart.target";
                Wants =
                  "graphical-session-pre.target xdg-desktop-autostart.target";
                After = "graphical-session-pre.target";
                X-RestartIfChanged = false;
              };
              Service = {
                Slice = "session.slice";
                Type = "notify";
                ExecStart =
                  "${niri}/bin/niri --session";
              };
            };
            systemd.user.targets.niri-shutdown = {
              Unit = {
                Description = "Shutdown running niri session";
                DefaultDependencies = "no";
                StopWhenUnneeded = "true";

                Conflicts =
                  "graphical-session.target graphical-session-pre.target";
                After = "graphical-session.target graphical-session-pre.target";
              };
            };
          })
        ];

        myconfig.desktop.wayland.sessions = {
          niri = { command = "${niri}/bin/niri-session"; };
          niri-plain = { command = "${niri}/bin/niri"; };
        };
      });
}
