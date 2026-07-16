# SPDX-License-Identifier: MIT
{
  pkgs,
  inputs,
  config,
  lib,
  myconfig,
  ...
}:
let
  nixosConfig = config;
  cfg = config.myconfig;
  user = myconfig.user;
  # Patch the upstream `niri-session` script which calls
  #   systemctl --user import-environment
  # without an explicit variable list. That bare form is deprecated by systemd
  # and on newer systemd versions aborts the session.
  # See https://github.com/niri-wm/niri/issues/254
  niri = pkgs.niri.overrideAttrs (old: {
    postInstall = (old.postInstall or "") + ''
      if [ -e "$out/bin/niri-session" ]; then
        substituteInPlace "$out/bin/niri-session" \
          --replace-fail \
            'systemctl --user import-environment' \
            'systemctl --user import-environment PATH WAYLAND_DISPLAY DISPLAY XDG_SESSION_TYPE XDG_CURRENT_DESKTOP XDG_SESSION_ID XDG_SESSION_DESKTOP XDG_SEAT XDG_VTNR'
      fi
    '';
  });
  uwsmCfg = cfg.desktop.wayland.uwsm;
  # Compositor entrypoint used when running niri under uwsm. uwsm itself sets
  # up the graphical-session systemd targets, so we launch `niri --session`
  # directly instead of the `niri-session` helper script.
  niriUwsmBin = pkgs.writeShellScript "niri-uwsm" ''
    exec ${niri}/bin/niri --session
  '';
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
  # Always expose the `extraBinds` home-manager option (even when niri is not
  # the active session) so other home-manager modules — e.g. voxtype — can
  # register niri keybinds. The value is substituted into the `binds { }`
  # block of the generated config at the `@@niri-extra-binds@@` marker.
  imports = [
    {
      home-manager.sharedModules = [
        {
          options.myconfig.desktop.wayland.niri.extraBinds = lib.mkOption {
            type = lib.types.str;
            default = "";
            description = ''
              Extra niri keybind lines (KDL, without the surrounding `binds { }`
              block) injected into the generated niri config. Intended for
              home-manager modules that want to register compositor binds.
            '';
          };
        }
      ];
    }
  ];
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
        # keyd disabled by default — it interferes with touchpad
        # "disable while typing". Re-enable on a per-host basis with
        # `services.keyd.enable = true;` if you want the launcher tap
        # and the F13 voxtype hotkey back.
        services.keyd = lib.mkIf config.services.keyd.enable {
          keyboards.default = {
            ids = [ "*" ];
            settings.global = {
              overload_tap_timeout = 200; # ms to register a tap before timeout
            };
            settings.main = {
              compose = "layer(meta)"; # hold Menu/Compose key for Super
              # tap Super → launcher (niri Mod+P → my-wofi); hold Super = modifier
              leftmeta = "overload(meta, macro(leftmeta+v))"; # NEO -> QWERTY
              # ^ key (below ESC, physical KEY_GRAVE) → F13. voxtype watches
              # evdev for F13 in push-to-talk mode (see
              # home-manager.services.voxtype.nix): hold to record, release to
              # transcribe. F13 is a function key, so the Neo XKB layout does
              # not affect it; the ^ dead-circumflex glyph is never produced.
              # keyd emits F13 on both press and release, giving voxtype a
              # clean down/up pair on its virtual device.
              grave = "f13";
            };
          };
        };
        # nixpkgs.overlays = [
        #   (_: _: {
        #     niri = inputs.niri.packages.${pkgs.system}.niri;
        #   })
        # ];
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
              xdg.configFile =
                let
                  extraBinds = config.myconfig.desktop.wayland.niri.extraBinds;
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

                        include "include.kdl"

                        spawn-at-startup "${niri-autostart}"
                        EOF
                        substituteInPlace $out/config.kdl \
                          --replace-fail "@@niri-extra-binds@@" ${lib.escapeShellArg extraBinds}
                        cat <<EOF >$out/include.kdl
                        ${cfg.desktop.wayland.niri.additionalConfigKdl}
                        EOF
                        niri validate --config $out/config.kdl > $out/config.kdl.validate
                      '';
                in
                {
                  "niri/config.kdl".source = "${drv}/config.kdl";
                  "niri/include.kdl".source = "${drv}/include.kdl";
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
          (
            { config, ... }:
            {
              programs.wlogout = {
                layout = [
                  {
                    label = "logout";
                    action = "${niri}/bin/niri msg action quit";
                    text = "Logout";
                    keybind = "e";
                  }
                ];
              };
            }
          )
        ];

        # Register niri as a uwsm-managed Wayland compositor. This produces the
        # `niri-uwsm.desktop` wayland-session entry (via programs.uwsm) used by
        # display managers, and is gated on uwsm being enabled.
        programs.uwsm.waylandCompositors = lib.mkIf uwsmCfg.enable {
          niri = {
            prettyName = "Niri";
            comment = "Niri compositor managed by UWSM";
            binPath = niriUwsmBin;
          };
        };

        myconfig.desktop.wayland.sessions = {
          # When uwsm is enabled this routes the greetd session through
          # `uwsm start -F -- niri --session`; otherwise it falls back to the
          # plain `niri-session` helper.
          niri = {
            command =
              if uwsmCfg.enable then
                uwsmCfg.mkSessionCommand { binPath = niriUwsmBin; }
              else
                "${niri}/bin/niri-session";
          };
          niri-plain = {
            command = "${niri}/bin/niri";
          };
        };
      }
  );
}
