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
      waybarDevelop = pkgs.writeShellScriptBin "waybarDevelop" ''
        set -euo pipefail
        css=~/myconfig/myconfig/modules/myconfig.desktop.wayland/programs.waybar/waybar.gtk.css
        config=~/.config/waybar/config
        ${pkgs.psmisc}/bin/pkill waybar || true
        cat <<EOF | ${pkgs.entr}/bin/entr -r ${waybarPackage}/bin/waybar --log-level debug -c $config -s $css
        $css
        EOF
      '';
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
      doesFileExistCheck =
        file:
        let
          bn = builtins.baseNameOf file;
          checkName = "custom/doesFileExistCheck#${bn}";
        in
        {
          modules-left = [ checkName ];
          "${checkName}" = {
            format = "{}";
            exec =
              (pkgs.writeShellScriptBin "doesFileExist" ''
                set -euo pipefail
                if [[ ! -f ${file} ]]; then
                  cat <<EOF
                {"text":"!${bn}","class":"error"}
                EOF
                fi
              '')
              + "/bin/doesFileExist";
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
              mainBar = lib.mkMerge (
                [
                  {
                    name = "mainBar";
                    layer = "top";
                    position = "left";
                    height = null;
                    spacing = 4;
                    modules-left = [ ];
                    modules-center = [
                      "idle_inhibitor"
                      "custom/test_for_missing_tb_changing"
                      # "custom/audio_idle_inhibitor"
                      "clock#time"
                      "clock#date"
                    ]
                    ++ lib.optionals power-profiles-daemon-config.enable [ "power-profiles-daemon" ]
                    ++ [
                      "custom/isvpn"
                      "systemd-failed-units#user"
                      "systemd-failed-units#system"
                    ];
                    modules-right = [
                      "battery"
                      "backlight"
                      "pulseaudio"
                      "tray"
                    ];
                    tray = {
                      spacing = 10;
                      rotate = 90;
                    };
                    "clock#time" = {
                      format = "<big>{:%H:%M<sub>:%S</sub>}</big>";
                      interval = 1;
                      tooltip = false;
                      rotate = 90;
                    };
                    "clock#date" = {
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
                      rotate = 90;
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
                      on-click-middle = "minimize-raise";
                      ignore-list = [
                        "Alacritty"
                        "Foot"
                        "foot"
                        "tfoot"
                      ];
                      app_ids-mapping = {
                        "firefoxdeveloperedition" = "firefox-developer-edition";
                      };
                      rotate = 90;
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
                      rotate = 90;
                    };
                    "systemd-failed-units#user" = {
                      "hide-on-ok" = true;
                      "format" = "✗ U:{nr_failed}";
                      "format-ok" = "✓";
                      "system" = false;
                      "user" = true;
                      rotate = 90;
                    };
                    "systemd-failed-units#system" = {
                      "hide-on-ok" = true;
                      "format" = "✗ S:{nr_failed}";
                      "format-ok" = "✓";
                      "system" = true;
                      "user" = false;
                      rotate = 90;
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
                      rotate = 90;
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
                      rotate = 90;
                    };
                    memory = {
                      format = "ram: {}%";
                      rotate = 90;
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
                      rotate = 90;
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
                      rotate = 90;
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
                      rotate = 90;
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
                      rotate = 90;
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
                      rotate = 90;
                    };
                    idle_inhibitor = {
                      format = "{icon}";
                      format-icons = {
                        activated = "";
                        deactivated = "";
                      };
                    };
                    "custom/audio_idle_inhibitor".rotate = 90;
                  }
                ]
                ++ (map doesFileExistCheck cfg.desktop.wayland.waybar.doesFileExistChecks)
              );
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
