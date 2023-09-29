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
            "dwl/tags"
            "sway/workspaces"
            "sway/mode"
          ];
          modules-center = [ "river/window" "sway/window" ];
          modules-right = [
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
            "idle_inhibitor"
            "tray"
            "clock"
          ];
          tray = { spacing = 10; };
          clock = {
            format = "{:%H:%M (%Y-%m-%d)}";
            tooltip-format = ''
              <big>{:%Y %B}</big>
              <tt><small>{calendar}</small></tt>'';
            format-alt = "{:%H:%M}";
          };
          cpu = {
            format = "cpu: {usage}%";
            tooltip = false;
            on-click = "foot-htop";
          };
          "river/tags" = {
            tag-labels = [ "U" "I" "A" "E" "O" "S" "N" "R" "T" ];
          };
          "dwl/tags" = {
            num-tags = 9;
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
      style = builtins.readFile ./waybar.gtk.css;
    };
  });
}
