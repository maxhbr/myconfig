# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, inputs, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
  hyprland = inputs.hyprland.packages.${pkgs.system}.hyprland;
  plugins = inputs.hyprland-plugins.packages.${pkgs.system};
in {
  options.myconfig = with lib; {
    desktop.wayland.hyprland = { enable = mkEnableOption "hyprland"; };
  };
  imports = [
    {
      programs.hyprland = {enable = false;};
    }
  ];
  config = (lib.mkIf
    (cfg.desktop.wayland.enable && cfg.desktop.wayland.hyprland.enable) {
      home-manager.sharedModules = [({config, ...}: let
        hyprctl = "${config.wayland.windowManager.hyprland.package}/bin/hyprctl";
        hyprctl-workspace = pkgs.writeShellScriptBin "hyprctl-workspace" ''
# SPDX-License-Identifier: CC0-1.0

set -euo pipefail

jq="${pkgs.jq}/bin/jq"
hyprctl="${hyprctl}"

get_number_of_monitors() {
    $jq -r '.|length'
}

get_active_monitor() {
    $jq -r '[.[]|select(.focused == true)][0].id'
}

get_passive_monitor_with_workspace() {
    local workspace="$1"
    $jq -r '[.[]|select(.focused == false and .activeWorkspace.id == '"$workspace"')][0].id'
}

switch_to_workspace() {
    local workspace="$1"
    if [[ $# -eq 2 ]]; then
        local activemonitor="$2"
        $hyprctl dispatch moveworkspacetomonitor "$workspace $activemonitor"
    fi
    $hyprctl dispatch workspace "$workspace"
}

swap_active_workspaces() {
    local activemonitor="$1"
    local passivemonitor="$2"
    $hyprctl dispatch swapactiveworkspaces "$activemonitor" "$passivemonitor"
}

main() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: $0 <workspace>" >&2
        exit 1
    fi
    local workspace=$1
    local monitors="$($hyprctl -j monitors)"

    if [[ $(echo "$monitors" | get_number_of_monitors) -eq 1 ]]; then
        switch_to_workspace "$workspace"
    fi

    activemonitor="$(echo "$monitors" | get_active_monitor)"
    passivemonitor="$(echo "$monitors" | get_passive_monitor_with_workspace "$workspace")"

    if [[ "$passivemonitor" == "null" ]]; then
        switch_to_workspace "$workspace" "$activemonitor"
    else
        swap_active_workspaces "$activemonitor" "$passivemonitor"
    fi
}

main "$@"
'';
        hyprctl-scripts = with pkgs; [
          hyprctl-workspace 
          (writeShellScriptBin "hyprctl-animations-off" "${hyprctl} keyword animations:enabled no")
          (writeShellScriptBin "hyprctl-animations-on"  "${hyprctl} keyword animations:enabled yes")
          (writeShellScriptBin "hyprctl-create-headless" ''
            ${hyprctl} output create headless
            echo "remove with:"
            echo "  hyprctl output remove HEADLESS-2"
          '')
        ];
      in {
        home.packages = with pkgs; [ 
          hyprpaper
          hyprnome
          hyprdim
        ] ++ hyprctl-scripts;
        # xdg.configFile."hypr/hyprpaper.conf".text = ''
        #   preload = 
        #   wallpaper = ${pkgs.hyprland}/share/backgrounds/hyprland.png
        # '';
        wayland.windowManager.hyprland = {
          enable = true;
          package = hyprland;
          extraConfig = ''
            source = ${./hypr/hyprland.conf}
            exec-once = ${
              pkgs.writeShellScriptBin "autostart.sh" ''
                set -x
                ${cfg.desktop.wayland.autostartCommands}
                pkill waybar ; ${config.programs.waybar.package}/bin/waybar > /tmp/hyprland.''${XDG_VTNR}.''${USER}.waybar.log 2>&1 &disown
              ''
            }/bin/autostart.sh
            # exec-once = ${pkgs.hyprdim}/bin/hyprdim
          '';
          plugins = with plugins; [
            # hyprwinwrap
          ];
        };
        programs.waybar = {
          enable = lib.mkDefault true;
          settings.mainBar = {
            modules-left =  [ "hyprland/workspaces" "hyprland/submap" ];
            modules-center = [ "hyprland/window" ];
            "hyprland/workspaces" = {
              "format" = "{icon}";
              "format-icons" = {
                "1"  = "u:<sub>1</sub>";
                "2"  = "i:<sub>2</sub>";
                "3"  = "a:<sub>3</sub>";
                "4"  = "e:<sub>4</sub>";
                "5"  = "o:<sub>5</sub>";
                "6"  = "s:<sub>6</sub>";
                "7"  = "n:<sub>7</sub>";
                "8"  = "r:<sub>8</sub>";
                "9"  = "t:<sub>9</sub>";
                "10" = "d:<sub>0</sub>";
              };
              # "persistent-workspaces" = {
              #   "*" = [ 1 2 3 4 5 6 7 8 9 10 ];
              # };
              "on-scroll-up" = "${pkgs.hyprnome}/bin/hyprnome";
              "on-scroll-down" = "${pkgs.hyprnome}/bin/hyprnome --previous";
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
            };
          };
        };
      })];
      myconfig.desktop.wayland.greetdSettings = {
        hyprland_session = {
          command = "${pkgs.hyprland}/bin/Hyprland";
          inherit user;
        };
      };
    });
}
