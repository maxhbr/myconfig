# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  options.myconfig = with lib; {
    desktop.wayland.hikari = {
      settings = mkOption {
        type = types.attrs;
        default = { };
        description = "Nix-style settings configuration for Hikari";
      };
      autostart = mkOption {
        type = types.lines;
        default = "";
        description = "Autostart script ran on Hikari startup";
      };
    };
  };
  config = (lib.mkIf
    (cfg.desktop.wayland.enable && builtins.elem "hikari" cfg.desktop.wayland.sessions) {
      myconfig.desktop.wayland.hikari = {
        autostart = ''
          set -x
          ${cfg.desktop.wayland.autostartCommands}
          pkill waybar ; waybar > /tmp/hikari.''${XDG_VTNR}.''${USER}.waybar.log 2>&1 &disown
          dbus-wm-environment hikari
        '';
        settings = {
          ui = {
            border = 1;
            gap = 5;
            step = 100;
            font = "BlexMono Nerd Font 10";

            # colorscheme = {
            #   selected = "0x3D1531";
            #   grouped = "0x301D2A";
            #   first = "0x503266";
            #   conflict = "0x961B25";
            #   insert = "0xE3C3FA";
            #   active = "0xAAAAAA";
            #   inactive = "0x260020";
            # };
          };

          # outputs = {
          #   "*" = {
          #     background = "${../wallpapers/clouds.png}";
          #   };
          # };

          actions = {
            terminal = "${pkgs.foot}/bin/foot";
            launcher = "${pkgs.wofi}/bin/wofi --show=drun";
            cmdlauncher = "${pkgs.wofi}/bin/wofi --show=run";
            screenshot = ''
              ${pkgs.slurp}/bin/slurp | ${pkgs.grim} -g - -t png \
              $HOME/Pictures/Screenshots/$(${pkgs.coreutils}/bin/date +%Y-%m-%d_%H-%M-%S).png
            '';
          };

          bindings = {
            keyboard = {
              "L+0" = "workspace-switch-to-sheet-0";
              "L+1" = "workspace-switch-to-sheet-1";
              "L+2" = "workspace-switch-to-sheet-2";
              "L+3" = "workspace-switch-to-sheet-3";
              "L+4" = "workspace-switch-to-sheet-4";
              "L+5" = "workspace-switch-to-sheet-5";
              "L+6" = "workspace-switch-to-sheet-6";
              "L+7" = "workspace-switch-to-sheet-7";
              "L+8" = "workspace-switch-to-sheet-8";
              "L+9" = "workspace-switch-to-sheet-9";
              "L+numbersign" = "workspace-switch-to-sheet-alternate";
              "L+Period" = "workspace-switch-to-sheet-current";
              "L+j" = "workspace-switch-to-sheet-next";
              "L+k" = "workspace-switch-to-sheet-prev";
              "L+Comma" = "workspace-switch-to-sheet-next-inhabited";
              "LS+Comma" = "workspace-switch-to-sheet-prev-inhabited";
              "LSC+g" = "workspace-show-group";
              "LSC+i" = "workspace-show-invisible";
              "LSC+Period" = "workspace-show-all";
              "LC+n" = "workspace-cycle-next";
              "LC+p" = "workspace-cycle-prev";

              "LC+i" = "sheet-show-invisible";
              "LC+Period" = "sheet-show-all";
              "LC+g" = "sheet-show-group";

              "LA+r" = "layout-reset";
              "LA+Return" = "layout-restack-append";
              "LAS+Return" = "layout-restack-prepend";
              "L+Home" = "layout-cycle-view-first";
              "L+End" = "layout-cycle-view-last";
              "L+n" = "layout-cycle-view-next";
              "L+p" = "layout-cycle-view-prev";
              "L+x" = "layout-exchange-view-next";
              "LS+x" = "layout-exchange-view-prev";
              "LA+x" = "layout-exchange-view-main";

              "LS+0" = "view-pin-to-sheet-0";
              "LS+1" = "view-pin-to-sheet-1";
              "LS+2" = "view-pin-to-sheet-2";
              "LS+3" = "view-pin-to-sheet-3";
              "LS+4" = "view-pin-to-sheet-4";
              "LS+5" = "view-pin-to-sheet-5";
              "LS+6" = "view-pin-to-sheet-6";
              "LS+7" = "view-pin-to-sheet-7";
              "LS+8" = "view-pin-to-sheet-8";
              "LS+9" = "view-pin-to-sheet-9";
              "LS+numbersign" = "view-pin-to-sheet-alternate";
              "LS+Period" = "view-pin-to-sheet-current";
              "LS+j" = "view-pin-to-sheet-next";
              "LS+k" = "view-pin-to-sheet-prev";

              "L+u" = "view-raise";
              "L+d" = "view-lower";
              "L+o" = "view-only";
              "L+h" = "view-hide";
              "LS+c" = "view-quit";
              "LS+n" = "view-cycle-next";
              "LS+p" = "view-cycle-prev";

              "L+Up" = "view-move-up";
              "L+Down" = "view-move-down";
              "L+Left" = "view-move-left";
              "L+Right" = "view-move-right";
              "LA+Up" = "view-decrease-size-up";
              "LAS+Up" = "view-increase-size-up";
              "LA+Down" = "view-increase-size-down";
              "LAS+Down" = "view-decrease-size-down";
              "LA+Left" = "view-decrease-size-left";
              "LAS+Left" = "view-increase-size-left";
              "LA+Right" = "view-increase-size-right";
              "LAS+Right" = "view-decrease-size-right";
              "LS+Up" = "view-snap-up";
              "LS+Down" = "view-snap-down";
              "LS+Left" = "view-snap-left";
              "LS+Right" = "view-snap-right";
              "L+r" = "view-reset-geometry";

              "L+minus" = "view-toggle-maximize-vertical";
              "L+less" = "view-toggle-maximize-horizontal";
              "L+f" = "view-toggle-maximize-full";
              "L5+plus" = "view-toggle-floating";
              "L+i" = "view-toggle-invisible";
              "L5+p" = "view-toggle-public";

              "LS+o" = "group-only";
              "LS+h" = "group-hide";
              "LS+u" = "group-raise";
              "LS+d" = "group-lower";
              "L+Tab" = "group-cycle-prev";
              "LS+Tab" = "group-cycle-next";
              "L+asciicircum" = "group-cycle-view-prev";
              "LS+asciicircum" = "group-cycle-view-next";
              "LS+Home" = "group-cycle-view-first";
              "LS+End" = "group-cycle-view-last";

              "L+l" = "mode-enter-layout";
              "L+s" = "mode-enter-sheet-assign";
              "L+g" = "mode-enter-group-assign";
              "L+m" = "mode-enter-mark-assign";
              "L+acute" = "mode-enter-mark-select";
              "LS+acute" = "mode-enter-mark-switch-select";
              "LCA+g" = "mode-enter-input-grab";

              "LS+Backspace" = "lock";
              "LCA+q" = "quit";
              "LCA+r" = "reload";

              "L+Return" = "action-terminal";
              "LS+Return" = "action-launcher";
              "LSA+Return" = "action-cmdlauncher";
              "L-107" = "action-screenshot";

              "CA+F1" = "vt-switch-to-1";
              "CA+F2" = "vt-switch-to-2";
              "CA+F3" = "vt-switch-to-3";
              "CA+F4" = "vt-switch-to-4";
              "CA+F5" = "vt-switch-to-5";
              "CA+F6" = "vt-switch-to-6";
              "CA+F7" = "vt-switch-to-7";
              "CA+F8" = "vt-switch-to-8";
              "CA+F9" = "vt-switch-to-9";
            };

            mouse = {
              "L+left" = "mode-enter-move";
              "L+right" = "mode-enter-resize";
            };
          };
          inputs = {
            pointers = {
              "*" = {
                accel = 0.3;
                accel-profile = "adaptive";
              };
            };
            keyboards = {
              "*" = {
                xkb = { layout = "de(neo)"; };
                repeat-rate = 25;
                repeat-delay = 600;
              };
            };
            switches = { "Lid Switch" = "lock"; };
          };
          layouts = {
            # main stack
            s = {
              scale = {
                min = 0.5;
                max = 0.75;
              };
              left = "single";
              right = "stack";
            };

            # main queue
            q = {
              scale = 0.75;
              top = "single";
              bottom = "queue";
            };

            # nautilus
            n = {
              left = "single";
              right = {
                top = "single";
                bottom = {
                  right = "single";
                  left = {
                    bottom = "single";
                    top = "full";
                  };
                };
              };
            };

            t = {
              scale = 0.2;
              top = "single";
              bottom = "single";
            };

            # main queue
            x = {
              scale = 0.5;
              left = "single";
              right = "stack";
            };

            f = "full";
            h = "stack";
            v = "queue";
            g = "grid";
          };
        };
      };
      home-manager.sharedModules = [{
        xdg.configFile = {
          "hikari/hikari.conf".source = let
            jsonFormat = pkgs.formats.json { };
            conf = jsonFormat.generate "hikari.conf"
              cfg.desktop.wayland.hikari.settings;
          in conf;
          "hikari/autostart".text = cfg.desktop.wayland.hikari.autostart;
        };
        home.packages = [ pkgs.hikari ];
      }];
      myconfig.desktop.wayland.greetdSettings = {
        hikari_session = {
          command = "${pkgs.hikari}/bin/hikari";
          inherit user;
        };
      };
    });
}
