{ config, lib, pkgs, ... }: {
  config = (lib.mkIf config.programs.waybar.enable {
    programs.waybar = {
      systemd.enable = false;
      settings = {
        mainBar = {
          height = 25;
          spacing = 4;
          modules-left = [
            # "river/mode"
            "river/tags"
            "sway/workspaces"
            "sway/mode"
          ];
          modules-center = [
            "river/window"
            "sway/window"
          ];
          modules-right = [
            "idle_inhibitor"
            "pulseaudio"
            "bluetooth"
            "network"
            "custom/isvpn"
            "custom/platform_profile"
            "custom/test_for_missing_tb_changing"
            # "cpu"
            # "memory"
            "backlight"
            "battery"
            "wlr/taskbar"
            "tray"
            "clock"
          ];
          tray = { spacing = 10; };
          clock = {
            format = "{:%H:%M}";
            tooltip-format = ''
              <big>{:%Y %B}</big>
              <tt><small>{calendar}</small></tt>'';
            format-alt = "{:%H:%M (%Y-%m-%d)}";
          };
          cpu = {
            format = "cpu: {usage}%";
            tooltip = false;
            on-click = "foot-htop";
          };
          "river/tags" = {
            tag-labels = [ "U" "I" "A" "E" "O" "S" "N" "R" "T" ];
          };
          "wlr/taskbar" = {
            format = "{icon}";
            icon-size = 14;
            icon-theme = "Numix-Circle";
            tooltip-format = ''
              {title}
              {name}
              {short_state}'';
            on-click = "activate";
            on-click-middle = "close";
            ignore-list = [ "Alacritty" "Foot" "foot" "tfoot" ];
            app_ids-mapping = {
              "firefoxdeveloperedition" = "firefox-developer-edition";
            };
          };
          "custom/platform_profile" = {
            format = "{}";
            exec = (pkgs.writeShellScriptBin "getPlatformProfile" ''
              profile="$(cat /sys/firmware/acpi/platform_profile)"
              cat <<EOF
              {"text":"$profile","class":"$profile"}
              EOF
            '') + "/bin/getPlatformProfile";
            exec-if =
              "! grep -q performance /sys/firmware/acpi/platform_profile";
            return-type = "json";
            interval = 5;
          };
          "custom/isvpn" = {
            format = "{}";
            exec = (pkgs.writeShellScriptBin "isvpn" ''
              if ${pkgs.nettools}/bin/ifconfig tun0 &> /dev/null; then
                cat <<EOF
              {"text":"VPN","class":"warning"}
              EOF
              fi
            '') + "/bin/isvpn";
            return-type = "json";
            interval = 30;
          };
          "custom/test_for_missing_tb_changing" = {
            format = "{}";
            exec = (pkgs.writeShellScriptBin "test_for_missing_tb_changing" ''
              if ${pkgs.bolt}/bin/boltctl list | ${pkgs.gnugrep}/bin/grep "status" | ${pkgs.gnugrep}/bin/grep -q -v "disconnected"; then
                  if ${pkgs.acpi}/bin/acpi -a | ${pkgs.gnugrep}/bin/grep -q "off-line"; then
                      cat <<EOF
              {"text":"not-charging","class":"error"}
              EOF
                     exit 0
                  fi
              fi
            '') + "/bin/test_for_missing_tb_changing";
            return-type = "json";
            interval = 60;
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
          bluetooth = {
            format = " {status}";
            format-connected = " {device_alias}";
            format-connected-battery =
              " {device_alias} {device_battery_percentage}%";
            # format-device-preference = [ "device1"; "device2" ], # preference list deciding the displayed device
            tooltip-format = ''
              {controller_alias}	{controller_address}

              {num_connections} connected'';
            tooltip-format-connected = ''
              {controller_alias}	{controller_address}

              {num_connections} connected

              {device_enumerate}'';
            tooltip-format-enumerate-connected =
              "{device_alias}	{device_address}";
            tooltip-format-enumerate-connected-battery =
              "{device_alias}	{device_address}	{device_battery_percentage}%";
            on-click = "foot-bluetuith";
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
          idle_inhibitor = {
            format = "{icon}";
            format-icons = {
              activated = "";
              deactivated = "";
            };
          };
        };
      };
      style = let 
        white = "#e0cbc7";
      in ''
        * {
            border: none;
            border-radius: 0;
            font-family: Roboto, Helvetica, Arial, sans-serif;
            font-size: 13px;
            min-height: 0;
        }

        window#waybar {
            background: rgba(43, 48, 59, 0.5);
            border-bottom: 3px solid rgba(195, 210, 222, 0.5); /* c3d2df */
            color: ${white};
        }

        tooltip {
          background: rgba(43, 48, 59, 0.5);
          border: 1px solid rgba(100, 114, 125, 0.5);
        }
        tooltip label {
          color: ${white};
        }

        #mode {
            padding: 0 10px;
            background: #64727d;
            border-bottom: 3px solid ${white};
        }

        #battery,
        #cpu,
        #memory,
        #disk,
        #backlight,
        #network,
        #pulseaudio,
        #bluetooth,
        #custom-platform_profile,
        #idle_inhibitor,
        #tray,
        #mode,
        #taskbar button,
        #window,
        #tags button {
            padding: 0 10px;
            border-radius: 0px 0px 7px 7px;
        }

        #clock {
            border-bottom: 3px solid ${white};
        }

        #taskbar button,
        #tags button,
        #workspaces button {
            padding: 0;
            margin: 0;
            background: transparent;
            color: ${white};
            border-bottom: 3px solid transparent;
        }
        #tags button * {
            margin: 0;
            padding: 0;
        }
        #taskbar button.active {
            background-color: #ffffff;
        }

        #battery {
            background-color: #ffffff;
            color: black;
        }

        #battery.charging {
            background-color: #26a65b;
            color: white; /* actual white */
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
        #battery.critical:not(.charging), #custom-test_for_missing_tb_changing.error {
            background: #f53c3c;
            color: white;
            animation-name: blink;
            animation-duration: 0.5s;
            animation-timing-function: linear;
            animation-iteration-count: infinite;
            animation-direction: alternate;
        }

        /* #taskbar button.minimized */
        /* #taskbar button.maximized */
        /* #taskbar button.fullscreen */

        #tags button.focused, #workspaces button.focused{
            background: #64727d;
            border-right: 3px solid ${white};
            border-left: 3px solid ${white};
        }
        #tray {
            border-left: 3px solid ${white};
            border-bottom: 3px solid ${white};
            border-radius: 0px 0px 0px 7px;
        }
        /* #taskbar button.maximized, */
        #tags button.occupied , #window.focused {
          border-bottom: 3px solid ${white};
        }
        #taskbar button.fullscreen,
        #tags button.urgent {
          border-bottom: 3px solid #ee9a00;
        }

        #idle_inhibitor {
          min-width: 15px;
        }
        #idle_inhibitor.deactivated {
            background-color: #1F2C36;
        }
      '';
    };
  });
}
