{ config, lib, pkgs, ... }: {
  config = (lib.mkIf config.programs.waybar.enable {
    programs.waybar = {
      systemd.enable = false;
      settings = {
        mainBar = {
          height = 25;
          spacing = 4;
          modules-left = [ "river/tags" "river/window"
                           "sway/workspaces" "sway/mode" "sway/window"
                         ];
          modules-center = [ ];
          modules-right = [
            "pulseaudio"
            "network"
            "cpu"
            "custom/platform_profile"
            "memory"
            "backlight"
            "battery"
            "clock"
            "tray"
          ];
          tray = { spacing = 10; };
          clock = {
            tooltip-format = ''
              <big>{:%Y %B}</big>
              <tt><small>{calendar}</small></tt>'';
            format-alt = "{:%Y-%m-%d}";
          };
          cpu = {
            format = "cpu: {usage}%";
            tooltip = false;
          };
          "custom/platform_profile" = {
            format = "{}";
            exec = (pkgs.writeShellScriptBin "getPlatformProfile" ''
profile="$(cat /sys/firmware/acpi/platform_profile)"
cat <<EOF
{"text":"$profile","class":"$profile"}
EOF
'') + "/bin/getPlatformProfile";
            exec-if = "! grep -q performance /sys/firmware/acpi/platform_profile";
            return-type = "json";
            interval = 5;
          };
          memory = { format = "ram: {}%"; };
          backlight = {
            format = "{percent}% {icon}";
            format-icons = [ "" "" "" "" "" "" "" "" "" ];
          };
          battery = {
            states = {
              warning = 40;
              critical = 20;
            };
            format = "{capacity}% {icon}";
            format-charging = "{capacity}% ";
            format-plugged = "{capacity}% ";
            format-alt = "{time} {icon}";
            format-icons = [ "" "" "" "" "" ];
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
              default = [ "" "" "" ];
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
        #custom-platform_profile,
        #tray,
        #mode {
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

        #custom-platform_profile.balanced {
            background: #ee9a00;
        }

        #custom-platform_profile.low-power {
            background: #f53c3c;
        }

        #battery.warning:not(.charging) {
            background: #ee9a00;
        }
        #battery.critical:not(.charging) {
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
  });
}