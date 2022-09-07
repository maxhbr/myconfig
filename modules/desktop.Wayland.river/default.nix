# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: 
let cfg = config.myconfig;
    riverPackage = pkgs.callPackage ./wrapper.nix {
      river-unwrapped = pkgs.river;
      withBaseWrapper = true;
      extraPaths= with pkgs; [
        (writeShellScriptBin "reriver" ''
sudo systemctl restart greetd.service
'')
        rivercarro
        ## Terminal
        foot
        # https://github.com/riverwm/river/wiki/Recommended-Software
        ## Output configuration
        wlopm way-displays # wlr-randr kanshi
        ## statusbar
        waybar
        ## Program Launchers
        bemenu fuzzel
        ## Screen Lockers
        swaylock
        ## Idle Management
        swayidle
        (writeShellScriptBin "myswayidle" ''
set -euo pipefail
${swayidle}/bin/swayidle -w \
  timeout 300 '${swaylock}/bin/swaylock -f -c 000000' \
  before-sleep '${swaylock}/bin/swaylock -f -c 000000'
'')
        ## Other
        ristate
        wayshot
      ];
      extraSessionCommands = ''
        export XKB_DEFAULT_LAYOUT=de
        export XKB_DEFAULT_VARIANT=neo
      '';
      withGtkWrapper = true;
      extraOptions = [];
    };
in {
  options.myconfig = with lib; { river.enable = mkEnableOption "river"; };
  config = (lib.mkIf cfg.river.enable {
    home-manager.sharedModules = [{
      xdg.configFile = {
        "river/init".source = ./river/init;
        "way-displays/cfg.yaml".source = ./way-displays/cfg.yaml;
        # "waybar/config".source = ./waybar/config;
      };
      home.packages = with pkgs; [
        riverPackage
      ];
      services.random-background.enable = lib.mkForce false;
      programs.waybar = {
        enable = true;
        systemd.enable = false;
        settings = {
          mainBar = {
            height = 25;
            spacing = 4;
            modules-left = ["river/tags" "river/window"];
            modules-center = [];
            modules-right = ["idle_inhibitor" "pulseaudio" "network" "cpu" "memory" "backlight" "sway/language" "battery" "clock" "tray"];
            idle_inhibitor = {
              format = "{icon}";
              format-icons = {
                activated = "";
                deactivated = "";
              };
            };
            tray = {
              spacing = 10;
            };
            clock = {
              tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
              format-alt = "{:%Y-%m-%d}";
            };
            cpu = {
              format = "cpu: {usage}%";
              tooltip = false;
            };
            memory = {
              format = "ram: {}%";
            };
            backlight = {
              format = "{percent}% {icon}";
              format-icons = ["" "" "" "" "" "" "" "" ""];
            };
            battery = {
              states = {
                warning = 30;
                critical = 15;
              };
              format = "{capacity}% {icon}";
              format-charging = "{capacity}% ";
              format-plugged = "{capacity}% ";
              format-alt = "{time} {icon}";
              format-icons = ["" "" "" "" ""];
            };
            network = {
              format-wifi = "{essid} ({signalStrength}%) ";
              format-ethernet = "{ipaddr}/{cidr} ";
              tooltip-format = "{ifname} via {gwaddr} ";
              format-linked = "{ifname} (No IP) ";
              format-disconnected = "Disconnected ⚠";
              format-alt = "{ifname}: {ipaddr}/{cidr}";
            };
            pulseaudio = {
              format = "{volume}% {icon} {format_source}";
              format-bluetooth = "{volume}% {icon} {format_source}";
              format-bluetooth-muted = " {icon} {format_source}";
              format-muted = " {format_source}";
              format-source = "{volume}% ";
              format-source-muted = "";
              format-icons = {
                headphone = "";
                hands-free = "";
                headset = "";
                phone = "";
                portable = "";
                car = "";
                default = ["" "" ""];
              };
              on-click = "pavucontrol";
            };
          };
        };
        style = ''
* {
    border: none;
    border-radius: 0;
    font-family: Roboto, Helvetica, Arial, sans-serif;
    font-size: 13px;
    min-height: 0;
}

window#waybar {
    background: rgba(43, 48, 59, 0.5);
    border-bottom: 3px solid rgba(100, 114, 125, 0.5);
    color: white;
}

tooltip {
  background: rgba(43, 48, 59, 0.5);
  border: 1px solid rgba(100, 114, 125, 0.5);
}
tooltip label {
  color: white;
}

#mode {
    padding: 0 10px;
    background: #64727D;
    border-bottom: 3px solid white;
}

#clock,
#battery,
#cpu,
#memory,
#disk,
#backlight,
#network,
#pulseaudio,
#custom-media,
#tray,
#mode,
#idle_inhibitor {
    padding: 0 10px;
    background-color: #64727D;
}

#battery {
    background-color: #ffffff;
    color: black;
}

#battery.charging {
    color: white;
    background-color: #26A65B;
}

@keyframes blink {
    to {
        background-color: #ffffff;
        color: black;
    }
}

#battery.warning:not(.charging) {
    background: #f53c3c;
    color: white;
    animation-name: blink;
    animation-duration: 0.5s;
    animation-timing-function: linear;
    animation-iteration-count: infinite;
    animation-direction: alternate;
}


#tags button, #workspaces button {
    padding: 0 5px;
    background: transparent;
    color: white;
    border-bottom: 3px solid transparent;
}

#tags button.focused, #workspaces button.focused {
    background: #64727D;
}
#tags button.occupied {
  border-bottom: 3px solid white;
}
#tags button.urgent {
  border-bottom: 3px solid #ee9a00;
}
'';
      };
    }];
    services= {
      xserver.displayManager.sddm = {
        settings = {
          General.DefaultSession = "river.desktop";
        };
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
  });
}
