# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  nixosConfig = config;
  cfg = config.myconfig;
  user = myconfig.user;
  niri = pkgs.niri;
in
{
  # add option for additional config added to config.kdl
  options.myconfig = with lib; {
    desktop.wayland.niri.additionalConfigKdl = lib.mkOption {
      type = types.str;
      default = "";
      description = "Additional config added to config.kdl";
    };
  };
  config = (
    lib.mkIf
      (
        cfg.desktop.wayland.enable
        && (
          builtins.elem "niri" cfg.desktop.wayland.selectedSessions
          || builtins.elem "niri-plain" cfg.desktop.wayland.selectedSessions
        )
      )
      {
        home-manager.sharedModules = [
          (
            { config, ... }:
            let
              niri-autostart = pkgs.writeShellScript "niri-autostart" ''
                set -x
                exec &> >(tee -a /tmp/niri.''${XDG_VTNR}.''${USER}.autostart.log)
                ${cfg.desktop.wayland.autostartCommands}
              '';
              niri-xwayland-satellite = pkgs.writeShellScriptBin "niri-xwayland-satellite" ''
                kill_running_xwayland_satellite=false
                if [[ "$#" -gt 0 && "$1" == "kill" ]]; then
                  kill_running_xwayland_satellite=true
                fi

                CHOSEN_DISPLAY="''${1:-"''${DISPLAY}"}"
                pidfile=/tmp/niri.''${XDG_VTNR}.''${USER}.xwayland-satellite.''${CHOSEN_DISPLAY}.pid
                exec &> >(tee -a /tmp/niri.''${XDG_VTNR}.''${USER}.xwayland-satellite.''${CHOSEN_DISPLAY}.log)

                if [[ -f $pidfile ]]; then
                  pid=$(cat $pidfile)
                  echo "old pid: $pid"
                  if [[ "$kill_running_xwayland_satellite" == "true" ]]; then
                    kill $pid || true
                  else
                    if ps -p $pid > /dev/null; then
                      echo "xwayland-satellite already running"
                      exit 0
                    fi
                  fi
                fi

                set -euo pipefail
                set -x
                ${pkgs.xwayland-satellite}/bin/xwayland-satellite ''${CHOSEN_DISPLAY}
                echo $$ > $pidfile
              '';
              niri-toggle-dpi-scale = pkgs.writeShellScriptBin "niri-toggle-dpi-scale" ''
                set -euo pipefail

                # "niri-toggle-dpi-scale" [scale] [output]

                target=""
                if [[ "$#" -gt 0 && "$1" =~ ^[0-9]*\.?[0-9]+$ ]]; then
                    target="$1"; shift
                fi

                OUTPUT="''${1:-eDP-1}"

                if [[ -z "$target" ]]; then
                    HI="2"
                    LO="1.3"

                    current_scale=$(${pkgs.niri}/bin/niri msg --json outputs \
                                    | ${pkgs.jq}/bin/jq -r --arg o "$OUTPUT" '.[$o].logical.scale')

                    if [[ "$current_scale" == "$HI"* ]]; then
                        target="$LO"
                    else
                        target="$HI"
                    fi
                fi

                set -x
                ${pkgs.niri}/bin/niri msg output "$OUTPUT" scale "$target"
              '';
            in
            {
              home.sessionVariables = {
                DISPLAY = ":0";
              };
              home.packages = [
                niri
                niri-xwayland-satellite
                niri-toggle-dpi-scale
              ];
              xdg.configFile = {
                "niri/config.kdl".source =
                  let
                    drv =
                      pkgs.runCommand "niri-config"
                        {
                          nativeBuildInputs = [ niri ];
                          src = ./config.kdl;
                        }
                        ''
                          mkdir $out
                          cat <<EOF >$out/config.kdl
                          environment {
                              QT_QPA_PLATFORM "wayland"
                              DISPLAY "${config.home.sessionVariables.DISPLAY}"
                              ELECTRON_OZONE_PLATFORM_HINT "${nixosConfig.environment.sessionVariables.ELECTRON_OZONE_PLATFORM_HINT}"
                          }

                          $(cat $src)

                          xwayland-satellite {
                              path "${pkgs.xwayland-satellite}/bin/xwayland-satellite"
                          }

                          ${cfg.desktop.wayland.niri.additionalConfigKdl}

                          spawn-at-startup "${niri-autostart}"
                          EOF
                          niri validate --config $out/config.kdl > $out/config.kdl.validate
                        '';
                  in
                  "${drv}/config.kdl";
              };
              programs.waybar.settings.mainBar = {
                "niri/workspaces" = {
                  "format" = "{index}";
                  rotate = 90;
                };
                modules-left = [ "niri/workspaces" ];
                # modules-center = [ ];
                # modules-right = [ "wlr/taskbar" ];
              };
              # A dedicated anchor for your compositor session
              systemd.user.services.niri = {
                Unit = {
                  Description = "A scrollable-tiling Wayland compositor";
                  BindsTo = "graphical-session.target";
                  Before = "graphical-session.target xdg-desktop-autostart.target";
                  Wants = "graphical-session-pre.target xdg-desktop-autostart.target";
                  After = "graphical-session-pre.target";
                  X-RestartIfChanged = false;
                };
                Service = {
                  Slice = "session.slice";
                  Type = "notify";
                  ExecStart = "${niri}/bin/niri --session";
                  Restart = "on-failure"; # optional, keeps it from exiting permanently on crashes
                };
              };
              systemd.user.targets.niri-shutdown = {
                Unit = {
                  Description = "Shutdown running niri session";
                  DefaultDependencies = "no";
                  StopWhenUnneeded = "true";

                  Conflicts = "graphical-session.target graphical-session-pre.target";
                  After = "graphical-session.target graphical-session-pre.target";
                };
              };
            }
          )
        ];

        myconfig.desktop.wayland.sessions = {
          niri = {
            command = "${niri}/bin/niri-session";
          };
          niri-plain = {
            command = "${niri}/bin/niri";
          };
        };
      }
  );
}
