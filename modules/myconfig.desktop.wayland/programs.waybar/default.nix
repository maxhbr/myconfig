{ config, lib, pkgs, ... }:
let
  # waybar-master = inputs.nixpkgs-wayland.packages.${pkgs.system}.waybar;
  power-profiles-daemon-config = config.services.power-profiles-daemon;
  cfg = config.myconfig;
  hmModule = ({ config, ... }:
    let
      waybarPackage = if false then
        pkgs.waybar.override {
          cavaSupport = config.programs.cava.enable;
          #   evdevSupport = true;
          #   experimentalPatches = true;
          hyprlandSupport =
            builtins.elem "hyprland" cfg.desktop.wayland.sessions;
          #   inputSupport = true;
          #   jackSupport = true;
          mpdSupport = config.services.mpd.enable;
          #   mprisSupport = true;
          #   nlSupport = true;
          #   pipewireSupport = true;
          #   pulseSupport = true;
          #   rfkillSupport = true;
          #   sndioSupport = true;
          swaySupport = builtins.elem "sway" cfg.desktop.wayland.sessions;
          #   traySupport = true;
          #   udevSupport = true;
          #   upowerSupport = true;
          #   wireplumberSupport = true;
        }
      else
        pkgs.waybar;
      waybarOnce = pkgs.writeShellScriptBin "waybarOnce" ''
        set -euo pipefail
        session="''${1:-"$DESKTOP_SESSION"}"; shift
        bn=/tmp/''${session}.''${XDG_VTNR:-x}.''${USER}.waybar
        pidfile=$bn.pid
        logfile=$bn.log

        if [ -f $pidfile ]; then
          pid=$(cat $pidfile)
          if kill -0 $pid &> /dev/null; then
            kill $pid
          fi
        fi

        echo "... waybar log is written to $logfile"
        ${waybarPackage}/bin/waybar "$@" > $logfile 2>&1 &
        echo $! > $pidfile
        wait
      '';
    in {
      config = (lib.mkIf config.programs.waybar.enable {
        home.packages = [ waybarOnce ];
        programs.waybar = {
          package = waybarPackage;
          systemd.enable = false;
          settings = {
            mainBar = lib.recursiveUpdate {
              layer = "top";
              position = "top";
              height = 25;
              spacing = 4;
              modules-left = [ ];
              modules-center = [
                # "wlr/taskbar"
                # "group/hardware"
                "idle_inhibitor"
                "battery"
              ] ++ lib.optionals power-profiles-daemon-config.enable
                [ "custom/platform_profile" ] ++ [
                  "custom/test_for_missing_tb_changing"
                  # "custom/audio_idle_inhibitor"
                  "clock#time"
                  "clock#date"
                ];
              modules-right = [
                "pulseaudio"
                "backlight"
                "custom/isvpn"
                "network"
                "cpu"
                # "memory"
                "tray"
              ];
              # "group/hardware" = {
              #   "orientation" = "vertical";
              #   "modules" = [ "cpu" "memory" ];
              # };
              tray = { spacing = 10; };
              "clock#time" = {
                format = "<big>{:%H:%M<sub>:%S</sub>}</big>";
                interval = 1;
                tooltip = false;
              };
              "clock#date" = {
                # format = "<sub>{:%Y-%m-%d}</sub>";
                format = ''
                  {:%e
                  <sub>%b</sub>}'';
                interval = 60;
                tooltip = false;
              };
              cpu = {
                format = "cpu: {usage}%";
                tooltip = false;
                on-click = "foot-htop";
              };
              "wlr/taskbar" = {
                format = "{icon}";
                icon-size = 14;
                icon-theme = "Numix-Circle";
                tooltip-format = ''
                  {title}
                  {name}
                  app_id: {app_id}
                  {short_state}'';
                on-click = "activate";
                on-click-middle = "close";
                ignore-list = [ "Alacritty" "Foot" "foot" "tfoot" ];
                app_ids-mapping = {
                  "firefoxdeveloperedition" = "firefox-developer-edition";
                };
              };
              "custom/platform_profile" =
                lib.mkIf power-profiles-daemon-config.enable {
                  format = "{}";
                  exec = (pkgs.writeShellScriptBin "getPlatformProfile" ''
                    profile="$(${power-profiles-daemon-config.package}/bin/powerprofilesctl get)"
                    cat <<EOF
                    {"text":"$profile","class":"$profile"}
                    EOF
                  '') + "/bin/getPlatformProfile";
                  exec-if =
                    "! grep -q performance /sys/firmware/acpi/platform_profile";
                  on-click =
                    "${power-profiles-daemon-config.package}/bin/powerprofilesctl set performance";
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
                exec =
                  (pkgs.writeShellScriptBin "test_for_missing_tb_changing" ''
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
                on-scroll-up = "${pkgs.light}/bin/light -A 1";
                on-scroll-down = "${pkgs.light}/bin/light -U 1";
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
                # format-icons = ["\uf244" "\uf243" "\uf242" "\uf241" "\uf240"];
              };
              network = {
                format-wifi = "{essid} ({signalStrength}%) ";
                format-ethernet = "{ifname} ";
                tooltip-format = ''
                  {ifname} via {gwaddr} 
                  {ipaddr}/{cidr}'';
                format-linked = "{ifname} (No IP) ";
                format-disconnected = "Disconnected ⚠";
                # format-alt = "{ifname}: {ipaddr}/{cidr}";
                on-click = "foot-nmtui";
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
            } {
              position = lib.mkForce "left";
              height = lib.mkForce null;
              pulseaudio.rotate = 90;
              bluetooth.rotate = 90;
              network.rotate = 90;
              "custom/isvpn.rotate" = 90;
              "custom/platform_profile".rotate = 90;
              "custom/test_for_missing_tb_changing".rotate = 90;
              cpu.rotate = 90;
              memory.rotate = 90;
              backlight.rotate = 90;
              battery.rotate = 90;
              "wlr/taskbar".rotate = 90;
              "group/hardware".rotate = 90;
              "custom/audio_idle_inhibitor".rotate = 90;
              tray.rotate = 90;
              clock.rotate = 90;
              "clock#time".rotate = 90;
            };
          };
          style = builtins.readFile ./waybar.gtk.css;
        };
      });
    });
in { config = { home-manager.sharedModules = [ hmModule ]; }; }
