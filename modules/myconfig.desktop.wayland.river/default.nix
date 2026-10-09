# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig;
  # river 0.4+ (pkgs.river) split the window manager out of the compositor and
  # is incompatible with the riverctl configuration in ./river/init. river-classic
  # is the maintained fork of river 0.3 with the same interface.
  riverPackage =
    let
      extraPaths = with pkgs; [
        rivercarro
        ristate
        swaybg
        kile-wl
      ];
    in
    pkgs.callPackage ./wrapper.nix {
      river-unwrapped = pkgs.river-classic;
      withBaseWrapper = true;
      inherit extraPaths;
      extraSessionCommands = ''
        export XDG_CURRENT_DESKTOP=river
        export XKB_DEFAULT_LAYOUT=${config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"}
        export XKB_DEFAULT_VARIANT=${config.environment.sessionVariables."XKB_DEFAULT_VARIANT"}
        export XDG_SESSION_TYPE=${config.environment.sessionVariables."XDG_SESSION_TYPE"}
        export SDL_VIDEODRIVER=${config.environment.sessionVariables."SDL_VIDEODRIVER"}
        export QT_QPA_PLATFORM=${config.environment.sessionVariables."QT_QPA_PLATFORM"}
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=${
          config.environment.sessionVariables."QT_WAYLAND_DISABLE_WINDOWDECORATION"
        }
        export ELECTRON_OZONE_PLATFORM_HINT=${
          config.environment.sessionVariables."ELECTRON_OZONE_PLATFORM_HINT"
        }
        export _JAVA_AWT_WM_NONREPARENTING=${
          config.environment.sessionVariables."_JAVA_AWT_WM_NONREPARENTING"
        }
      '';
      withGtkWrapper = true;
      extraOptions = [ ];
    };
  # The init script with the @@river-extra-init@@ marker replaced by
  # myconfig.desktop.wayland.river.additionalInit.
  riverInit =
    let
      additionalInit = cfg.desktop.wayland.river.additionalInit;
    in
    pkgs.runCommand "river-init"
      {
        src = ./river/init;
        nativeBuildInputs = [ pkgs.shellcheck ];
      }
      ''
        mkdir -p $out/bin
        substituteAllInPlace $src
        substituteInPlace $src \
          --replace-fail "@@river-extra-init@@" ${lib.escapeShellArg additionalInit}
        install -m755 $src $out/bin/init
        shellcheck -x $out/bin/init
        mv $out/bin/init $out/bin/river-init
      '';
  uwsmCfg = cfg.desktop.wayland.uwsm;
  # Compositor entrypoint used when running river under uwsm.
  riverUwsmBin = pkgs.writeShellScript "river-uwsm" ''
    exec ${riverPackage}/bin/river
  '';
in
{
  options.myconfig = with lib; {
    desktop.wayland.river.additionalInit = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Additional `riverctl` lines appended to the generated river init
        script before the layout generator is started. See riverctl(1).
      '';
    };
  };

  config = (
    lib.mkIf (cfg.desktop.wayland.enable && builtins.elem "river" cfg.desktop.wayland.selectedSessions)
      {
        home-manager.sharedModules = [
          (
            { config, ... }:
            {
              xdg.configFile = {
                "river/init".source = "${riverInit}/bin/river-init";
                "river/init".executable = true;
                "river/autostart" = {
                  source =
                    (pkgs.writeShellScriptBin "autostart.sh" ''
                      set -x
                      ${cfg.desktop.wayland.autostartCommands}

                      waybarOnce --auto-restart river &disown
                    '')
                    + "/bin/autostart.sh";
                  executable = true;
                };
              };
              home.packages = [ riverPackage ];
              programs.waybar.settings.mainBar = {
                modules-left = [ "river/tags" ];
                "river/tags" = {
                  tag-labels = [
                    "U"
                    "I"
                    "A"
                    "E"
                    "O"
                    "S"
                    "N"
                    "R"
                    "T"
                  ];
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
                    action = "riverctl exit";
                    text = "Logout";
                    keybind = "e";
                  }
                ];
              };
            }
          )
        ];

        # Register river as a uwsm-managed Wayland compositor. This produces
        # the `river-uwsm.desktop` wayland-session entry (via programs.uwsm)
        # used by display managers, and is gated on uwsm being enabled.
        programs.uwsm.waylandCompositors = lib.mkIf uwsmCfg.enable {
          river = {
            prettyName = "River";
            comment = "River compositor managed by UWSM";
            binPath = riverUwsmBin;
          };
        };

        myconfig.desktop.wayland.sessions = {
          river = {
            command =
              if uwsmCfg.enable then
                uwsmCfg.mkSessionCommand { binPath = riverUwsmBin; }
              else
                "${riverPackage}/bin/river";
          };
        };
      }
  );
}
