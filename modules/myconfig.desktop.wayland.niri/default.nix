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
  niriPkgs = pkgs.extend inputs.niri.overlays.default;
  niri = niriPkgs.niri.overrideAttrs (old: {
    postInstall = (old.postInstall or "") + ''
      if [ -e "$out/bin/niri-session" ]; then
        substituteInPlace "$out/bin/niri-session" \
          --replace-fail \
            'systemctl --user import-environment' \
            'systemctl --user import-environment PATH WAYLAND_DISPLAY DISPLAY XDG_SESSION_TYPE XDG_CURRENT_DESKTOP XDG_SESSION_ID XDG_SESSION_DESKTOP XDG_SEAT XDG_VTNR'
      fi
    '';
  });
  niriRunOrRaise = pkgs.writeShellApplication {
    name = "niri-run-or-raise";
    runtimeInputs = [
      niri
      pkgs.jq
    ];
    text = builtins.readFile ./bin/niri-run-or-raise.sh;
  };
  headless = cfg.headless.enable;
  headlessWindowRules = lib.optionalString headless ''
    // headless (myconfig.headless.enable): the initial foot terminal hosts
    // the main/host agent session (see myconfig.desktop.wayland.autostartCommands),
    // open it fullscreen instead of tiled.
    window-rule {
        match app-id="^foot-host-tmux-session$|^host-tmux-session$"

        open-fullscreen true
    }
  '';
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
  imports = [
    {
      home-manager.sharedModules = [
        {
          options.myconfig.desktop.wayland.niri.extraBinds = lib.mkOption {
            type = lib.types.lines;
            default = "";
            description = ''
              Extra niri keybind lines (KDL, without the surrounding `binds { }`
              block) injected into the generated niri config. Intended for
              home-manager modules that want to register compositor binds;
              `types.lines` lets several modules contribute binds.
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
        # Launcher entries listed in `launcherRunOrRaise` are dispatched to
        # `niri-run-or-raise`, so picking such an application focuses its
        # running window instead of starting a second instance.
        myconfig.desktop.wayland.launcherRunOrRaiseCommand = "${niriRunOrRaise}/bin/niri-run-or-raise";
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

                    current_scale=$(${niri}/bin/niri msg --json outputs \
                                    | ${pkgs.jq}/bin/jq -r --arg o "$OUTPUT" '.[$o].logical.scale')

                    if [[ "$current_scale" == "$HI"* ]]; then
                        target="$LO"
                    else
                        target="$HI"
                    fi
                fi

                set -x
                ${niri}/bin/niri msg output "$OUTPUT" scale "$target"
              '';
              kdlSource = builtins.readFile ./config.kdl;
              niriConfigKdl =
                lib.throwIfNot (lib.hasInfix "@@niri-extra-binds@@" kdlSource)
                  "@@niri-extra-binds@@ marker missing from config.kdl"
                  (
                    "environment {\n"
                    + "    QT_QPA_PLATFORM \"wayland\"\n"
                    + "    ELECTRON_OZONE_PLATFORM_HINT \"${nixosConfig.environment.sessionVariables.ELECTRON_OZONE_PLATFORM_HINT}\"\n"
                    + "}\n\n"
                    + (lib.replaceString "@@niri-extra-binds@@" config.myconfig.desktop.wayland.niri.extraBinds
                      kdlSource
                    )
                    + "\n"
                    + headlessWindowRules
                    + "include optional=true \"include.kdl\"\n\n"
                    + "spawn-at-startup \"${niri-autostart}\""
                  );
              niri-config =
                pkgs.runCommand "niri-config"
                  {
                    nativeBuildInputs = [ niri ];
                  }
                  ''
                    mkdir $out
                    printf '%s' ${lib.escapeShellArg niriConfigKdl} > $out/config.kdl
                    printf '%s' ${
                      lib.escapeShellArg (cfg.desktop.wayland.niri.additionalConfigKdl + "\n")
                    } > $out/include.kdl
                    niri validate --config $out/config.kdl
                  '';
            in
            {
              wayland.windowManager.niri = {
                enable = true;
                package = niri;
                portalPackage = pkgs.xdg-desktop-portal-gnome;
                systemd.enable = true;
                extraConfig = niriConfigKdl;
              };
              xdg.configFile."niri/include.kdl".source = "${niri-config}/include.kdl";
              home.packages = [
                niri-xwayland-satellite
                niri-toggle-dpi-scale
                niriRunOrRaise
              ];
              programs.waybar.settings.mainBar = {
                "niri/workspaces" = {
                  "format" = "{index}";
                  rotate = 90;
                };
                modules-left = [ "niri/workspaces" ];
              };
              xdg.configFile."systemd/user/niri.service.d/override.conf".text = ''
                [Unit]
                X-RestartIfChanged=false

                [Service]
                Restart=on-failure
              '';
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

        programs.uwsm.waylandCompositors = lib.mkIf uwsmCfg.enable {
          niri = {
            prettyName = "Niri";
            comment = "Niri compositor managed by UWSM";
            binPath = niriUwsmBin;
          };
        };

        myconfig.desktop.wayland.sessions = {
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
