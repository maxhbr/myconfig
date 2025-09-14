{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  # waybar-master = inputs.nixpkgs-wayland.packages.${pkgs.system}.waybar;
  power-profiles-daemon-config = config.services.power-profiles-daemon;
  cfg = config.myconfig;
  user = myconfig.user;
  hmModule = (
    { config, ... }:
    let
      waybarPackage =
        if false then
          pkgs.waybar.override {
            cavaSupport = config.programs.cava.enable;
            #   evdevSupport = true;
            #   experimentalPatches = true;
            hyprlandSupport = builtins.elem "hyprland" cfg.desktop.wayland.sessions;
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

        # Argument Parsing
        #  $0 [--auto-restart] [--no-auto-kill] session
        auto_restart=false
        auto_kill=true
        session=""
        while [[ $# -gt 0 ]]; do
          case "$1" in
            --auto-restart)
              auto_restart=true
              shift
              ;;
            --no-auto-kill)
              auto_kill=false
              shift
              ;;
            *)
              session="$1"
              shift
              break
              ;;
          esac
        done

        bn=/tmp/''${session}.''${XDG_VTNR:-x}.''${USER}.waybar
        pidfile=$bn.pid
        logfile=$bn.log

        auto_restart_waybar() {
          if [ -f $pidfile ]; then
            pid=$(cat $pidfile)
            if kill -0 $pid &> /dev/null; then
              echo "waybar is running, skipping auto-restart"
              exit 1
            fi
          fi
          set +e
          while true; do
            $0 --no-auto-kill "$session" "$@"
            echo "waybar crashed, restarting..."
            sleep 1
          done
          exit 0
        }

        start_waybar() {
          if [ -f $pidfile ]; then
            pid=$(cat $pidfile)
            if kill -0 $pid &> /dev/null; then
              if $auto_kill; then
                kill $pid
              else
                echo "waybar is running"
                exit 1
              fi
            else
              rm $pidfile
            fi
          fi

          echo "... waybar log is written to $logfile"
          ${waybarPackage}/bin/waybar "$@" > $logfile 2>&1 &
          echo $! > $pidfile
          wait
        }

        if $auto_restart; then
          auto_restart_waybar "$@"
        else
          start_waybar "$@"
        fi
      '';
      toggleLight = pkgs.writeShellScriptBin "toggleLight" ''
        set -x
        current=$(${pkgs.light}/bin/light -G)
        if [[ $current == "80.00"* ]]; then
          ${pkgs.light}/bin/light -S 20
        else
          ${pkgs.light}/bin/light -S 80
        fi
      '';
      doesFileExistCheck = file: let
          bn = builtins.baseNameOf file;
          checkName = "custom/doesFileExistCheck#${bn}";
        in {
        modules-left = [checkName];
        "${checkName}" = {
          format = "{}";
          exec = (pkgs.writeShellScriptBin "doesFileExist" ''
        set -euo pipefail
        if [[ ! -f ${file} ]]; then
          cat <<EOF
        {"text":"!${bn}","class":"error"}
        EOF
        fi
      '') + "/bin/doesFileExist";
          return-type = "json";
          interval = 6;
          rotation = 90;
        };
      };
    in
    {
      config = (
        lib.mkIf config.programs.waybar.enable {
          home.packages = [ waybarOnce ];
          programs.waybar = {
            package = waybarPackage;
            systemd.enable = false;
            settings = {
              mainBar =
                lib.mkMerge ([
                  {
                    layer = "top";
                    position = "top";
                    height = 25;
                    spacing = 4;
                    modules-left = [ ];
                    modules-center = [
                      # "wlr/taskbar"
                      # "group/hardware"
                      "idle_inhibitor"
                      "custom/test_for_missing_tb_changing"
                      # "custom/audio_idle_inhibitor"
                      "clock#time"
                      "clock#date"
                      # ];
                      # modules-right = [
                    ]
                    ++ lib.optionals power-profiles-daemon-config.enable [ "power-profiles-daemon" ]
                    ++ [
                      "battery"
                      "backlight"
                      "pulseaudio"
                      "systemd-failed-units#user"
                      "systemd-failed-units#system"
                      "custom/isvpn"
                      # "network"
                      # "cpu"
                      # "memory"
                      "tray"
                    ];
                    # "group/hardware" = {
                    #   "orientation" = "vertical";
                    #   "modules" = [ "cpu" "memory" ];
                    # };
                    tray = {
                      spacing = 10;
                    };
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
                      on-click = "foot-btop";
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
                      ignore-list = [
                        "Alacritty"
                        "Foot"
                        "foot"
                        "tfoot"
                      ];
                      app_ids-mapping = {
                        "firefoxdeveloperedition" = "firefox-developer-edition";
                      };
                    };
                    "power-profiles-daemon" = lib.mkIf power-profiles-daemon-config.enable {
                      "format" = "{icon}";
                      "tooltip-format" = ''
                        Power profile: {profile}
                        Driver: {driver}'';
                      "tooltip" = true;
                      "format-icons" = {
                        "default" = "";
                        "performance" = "";
                        "balanced" = "";
                        "power-saver" = "";
                      };
                    };
                    "systemd-failed-units#user" = {
                      "hide-on-ok" = true;
                      "format" = "✗ U:{nr_failed}";
                      "format-ok" = "✓";
                      "system" = false;
                      "user" = true;
                    };
                    "systemd-failed-units#system" = {
                      "hide-on-ok" = true;
                      "format" = "✗ S:{nr_failed}";
                      "format-ok" = "✓";
                      "system" = true;
                      "user" = false;
                    };
                    "custom/isvpn" = {
                      format = "{}";
                      exec =
                        (pkgs.writeShellScriptBin "isvpn" ''
                          if ${pkgs.nettools}/bin/ifconfig tun0 &> /dev/null; then
                            cat <<EOF
                          {"text":"VPN","class":"warning"}
                          EOF
                          fi
                        '')
                        + "/bin/isvpn";
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
                        '')
                        + "/bin/test_for_missing_tb_changing";
                      return-type = "json";
                      interval = 60;
                    };
                    memory = {
                      format = "ram: {}%";
                    };
                    backlight = {
                      format = "{percent}% {icon}";
                      format-icons = [
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                      ];
                      on-scroll-up = "${pkgs.light}/bin/light -A 1";
                      on-scroll-down = "${pkgs.light}/bin/light -U 1";
                      on-click = "${toggleLight}/bin/toggleLight";
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
                      format-icons = [
                        ""
                        ""
                        ""
                        ""
                        ""
                      ];
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
                      format-connected-battery = " {device_alias} {device_battery_percentage}%";
                      # format-device-preference = [ "device1"; "device2" ], # preference list deciding the displayed device
                      tooltip-format = ''
                        {controller_alias}	{controller_address}

                        {num_connections} connected'';
                      tooltip-format-connected = ''
                        {controller_alias}	{controller_address}

                        {num_connections} connected

                        {device_enumerate}'';
                      tooltip-format-enumerate-connected = "{device_alias}	{device_address}";
                      tooltip-format-enumerate-connected-battery = "{device_alias}	{device_address}	{device_battery_percentage}%";
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
                        default = [
                          ""
                          ""
                          ""
                        ];
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
                  }
                  {
                    position = lib.mkForce "left";
                    height = lib.mkForce null;
                    pulseaudio.rotate = 90;
                    bluetooth.rotate = 90;
                    network.rotate = 90;
                    "custom/isvpn.rotate" = 90;
                    "power-profiles-daemon".rotate = 90;
                    "systemd-failed-units".rotate = 90;
                    "systemd-failed-units#user".rotate = 90;
                    "systemd-failed-units#system".rotate = 90;
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
                  }
                ] ++ (map doesFileExistCheck cfg.desktop.wayland.waybar.doesFileExistChecks));
            };
            style = builtins.readFile ./waybar.gtk.css;
          };
        }
      );
    }
  );
in
{
  options.myconfig = with lib; {
    desktop.wayland = {
      waybar.doesFileExistChecks = mkOption {
        type = types.listOf types.str;
        default = [ ];
      };
    };
  };
  config = {
    home-manager.sharedModules = [ hmModule ];
    myconfig.desktop.wayland.waybar.doesFileExistChecks = [
      "/home/${user}/.home-manager-${user}.service.ready"
    ];
  };
}
