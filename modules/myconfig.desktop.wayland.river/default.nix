# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, inputs, ... }:
let
  cfg = config.myconfig;
  useMaster = true;
  river-master = pkgs.river.overrideAttrs
    (oa: {
      version = "master";

      src = inputs.river-src;

      buildInputs = with pkgs; [
        scdoc
        udev
        libevdev
        libinput
        pixman
        wlroots_0_17
        wayland-protocols
        libxkbcommon
      ];
    });
  rivercarro-master = pkgs.rivercarro.overrideAttrs (oa:
    {
      version = "master";
      src = inputs.rivercarro-src;
    }
  );
  wrapPackage = true;
  extraPaths = with pkgs; [ (if useMaster then rivercarro-master else rivercarro) ristate swaybg kile-wl ];
  riverPackage = if wrapPackage then
    pkgs.callPackage ./wrapper.nix {
      river-unwrapped = if useMaster then river-master else pkgs.river;
      withBaseWrapper = true;
      inherit extraPaths;
      extraSessionCommands = ''
        export XDG_CURRENT_DESKTOP=river
        export XKB_DEFAULT_LAYOUT=${
          config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"
        }
        export XKB_DEFAULT_VARIANT=${
          config.environment.sessionVariables."XKB_DEFAULT_VARIANT"
        }
        export XDG_SESSION_TYPE=${
          config.environment.sessionVariables."XDG_SESSION_TYPE"
        }
        export SDL_VIDEODRIVER=${
          config.environment.sessionVariables."SDL_VIDEODRIVER"
        }
        export QT_QPA_PLATFORM=${
          config.environment.sessionVariables."QT_QPA_PLATFORM"
        }
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=${
          config.environment.sessionVariables."QT_WAYLAND_DISABLE_WINDOWDECORATION"
        }
        export _JAVA_AWT_WM_NONREPARENTING=${
          config.environment.sessionVariables."_JAVA_AWT_WM_NONREPARENTING"
        }
      '';
      withGtkWrapper = true;
      extraOptions = [ ];
    }
  else
    pkgs.river;
  riverinit = pkgs.writeShellScriptBin "river-init" ''
    $HOME/.config/river/init &disown
  '';
in {
  options.myconfig = with lib; {
    desktop.wayland.river = { enable = mkEnableOption "river"; };
  };
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.river.enable) {
      home-manager.sharedModules = [
        ({ config, ... }: {
          xdg.configFile = {
            "river/init".source = ./river/init;
            "river/init".executable = true;
            # "river/init".onChange = "${pkgs.procps}/bin/pkill -u $USER river || true";
            "river/autostart" = {
              source = (pkgs.writeShellScriptBin "autostart.sh" ''
                set -x
                ${cfg.desktop.wayland.autostartCommands}
                pkill waybar ; waybar > /tmp/river.''${XDG_VTNR}.''${USER}.waybar.log 2>&1 &disown

                # TODO: why is/was river called in rivers autostart script, which is called by river??
                # # or: dbus-run-session -- river
                # dbus-wm-environment ${riverPackage}/bin/river
              '') + "/bin/autostart.sh";
              executable = true;
            };
            "river/kile-layout".source = ./river/kile-layout;
            "river/kile-layout".executable = true;
          };
          home.packages = with pkgs;
            [ riverPackage riverinit ]
            ++ (if wrapPackage then [ ] else extraPaths);
          programs.waybar.settings.mainBar = {
            modules-left = 
              [
                # "river/mode"
                "river/tags"
              ];
            # modules-center = [ "river/window" ];
            "river/tags" = {
              tag-labels = [ "U" "I" "A" "E" "O" "S" "N" "R" "T" ];
            };
          };
        })
      ];

      myconfig.desktop.wayland.greetdSettings = {
        river_session = {
          command = "${riverPackage}/bin/river";
          user = "mhuber";
        };
      };
    });
}
