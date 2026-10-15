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
  # Build upstream niri against the host libc so it can load the system Mesa driver.
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
  # On headless hosts (SSH + vnc/ast-vnc-style access, see
  # modules/myconfig.headless.nix) there is no physical monitor to size
  # windows against, so the initial foot terminal — the one autostarted as
  # `foot-host-tmux-session`/`host-tmux-session` in
  # myconfig.desktop.wayland.autostartCommands and hosting the main/host
  # agent session — should just take over the whole output instead of
  # tiling at some fraction of it.
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
  # Dynamic workspace naming: see ./workspace-names.nix and
  # doc/niri-dynamic-workspace-names.md.
  wsNamesCfg = cfg.desktop.wayland.niri.workspaceNames;
  wsNames = (import ./workspace-names.nix { inherit lib pkgs; }).niri-ws-names {
    inherit (wsNamesCfg) rules interval;
    inherit niri;
  };
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
    desktop.wayland.niri.workspaceNames = {
      enable = mkEnableOption "dynamic workspace naming for niri";
      rules = mkOption {
        # The daemon passes rules as a TSV file, so fields must not
        # contain tabs or newlines.
        type = types.listOf (
          types.submodule {
            options = {
              app-id = mkOption {
                type = types.strMatching "[^\t\n\r]*";
                description = "POSIX ERE matched against the window app-id";
                default = ".*";
              };
              title = mkOption {
                type = types.strMatching "[^\t\n\r]*";
                description = "POSIX ERE matched against the window title";
                default = ".*";
              };
              name = mkOption {
                type = types.strMatching "[^\t\n\r]*";
                description = "Workspace name to apply when the rule matches";
              };
            };
          }
        );
        default = [ ];
        description = ''
          Rules naming workspaces by their windows, evaluated in order
          (first match wins): a workspace holding any window that matches
          app-id and title is named `name`. A second workspace wanting the
          same name gets `name_2`, and so on.
        '';
      };
      interval = mkOption {
        type = types.ints.positive;
        default = 5;
        description = "Poll interval (seconds) of the renaming daemon";
      };
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
        #     niri = inputs.niri.packages.${pkgs.stdenv.hostPlatform.system}.niri;
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
              # The bulk of the niri config (binds, window rules, layout) lives
              # in config.kdl and is passed to home-manager's
              # `wayland.windowManager.niri` module via its `extraConfig`
              # verbatim passthrough. It contains `@@niri-extra-binds@@`
              # substitution markers and host-derived fragments (headless
              # window rules, NixOS session variables) which the typed
              # `settings` tree cannot express faithfully.
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
                    + "xwayland-satellite {\n"
                    + "    path \"${pkgs.xwayland-satellite}/bin/xwayland-satellite\"\n"
                    + "}\n\n"
                    # `optional=true` so that home-manager's build-time
                    # `niri validate` (checkConfig) tolerates the missing relative
                    # include; at runtime the file exists next to config.kdl.
                    + "include optional=true \"include.kdl\"\n\n"
                    + "spawn-at-startup \"${niri-autostart}\""
                    + lib.optionalString wsNamesCfg.enable "\nspawn-at-startup \"systemctl --user start niri-ws-names.service\""
                  );
              # The host-specific overrides (additionalConfigKdl) are pulled in
              # via the `include optional=true \"include.kdl\"` node above.
              # home-manager only validates config.kdl itself, so validate the
              # merged config (config.kdl + include.kdl) here and link the
              # result into place.
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
              # Drive home-manager's niri module
              # (modules/services/window-managers/niri.nix of the home
              # input). The generated config is validated by the module's
              # `checkConfig` (runs `niri validate` on the written file).
              wayland.windowManager.niri = {
                enable = true;
                package = niri;
                # xwayland-satellite is spawned by niri via the explicit
                # `xwayland-satellite { path ... }` node in config.kdl, so it
                # does not need to end up in PATH.
                xwaylandSatellitePackage = null;
                # Portals are configured at the NixOS level
                # (modules/services.dbus.nix), not via home-manager.
                portalPackage = null;
                # The niri systemd units are defined manually below (they add
                # a restart policy). Installing the upstream units as well
                # would collide with the hand-written niri.service.
                systemd.enable = false;
                extraConfig = niriConfigKdl;
              };
              # host-specific overrides (`additionalConfigKdl`) are pulled in
              # via `include optional=true "include.kdl"` in config.kdl
              xdg.configFile."niri/include.kdl".source = "${niri-config}/include.kdl";
              # Let niri's integrated Xwayland-satellite manage DISPLAY.
              # A global DISPLAY also makes TTY launches select the nested backend.
              home.packages = [
                niri-xwayland-satellite
                niri-toggle-dpi-scale
              ];
              programs.waybar.settings.mainBar = {
                "niri/workspaces" = {
                  # {value} shows the workspace name if it has one, its
                  # index otherwise (waybar niri-workspaces module).
                  "format" = if wsNamesCfg.enable then "{value}" else "{index}";
                  rotate = 90;
                };
                modules-left = [ "niri/workspaces" ];
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
              systemd.user.services.niri-ws-names = lib.mkIf wsNamesCfg.enable {
                Unit = {
                  Description = "Dynamic workspace naming for niri";
                  # The daemon polls niri via $NIRI_SOCKET, whose path embeds
                  # niri's PID: it is only valid for this session. Bind to the
                  # niri service so the daemon dies with the compositor
                  # instead of polling a stale socket forever.
                  BindsTo = [
                    "niri.service"
                    "graphical-session.target"
                  ];
                  PartOf = [ "graphical-session.target" ];
                  After = [
                    "niri.service"
                    "graphical-session.target"
                  ];
                  X-RestartIfChanged = false;
                };
                Service = {
                  Slice = "session.slice";
                  ExecStart = "${wsNames}/bin/niri-ws-names";
                  Restart = "on-failure";
                  # The step script is idempotent; a wedged call must not
                  # wedge the whole daemon.
                  TimeoutStopSec = 5;
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
