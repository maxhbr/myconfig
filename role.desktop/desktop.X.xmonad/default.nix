# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  my-mute-telco = pkgs.callPackage ./myMuteTelco { inherit pkgs; };
  my-xmobar = pkgs.callPackage ./myXmobar { inherit pkgs my-mute-telco; };
  my-xmonad = pkgs.haskellPackages.callPackage ./myXMonad {
    inherit pkgs my-xmobar my-mute-telco;
  };
  myxev = pkgs.writeShellScriptBin "myxev" ''
    ${pkgs.xorg.xev}/bin/xev -id $(${pkgs.xdotool}/bin/xdotool getactivewindow)
  '';

in {
  imports = [ ../desktop.X.common ];

  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        my-xmonad
        my-xmobar
        my-mute-telco

        dzen2
        myxev
      ];
      xsession.windowManager.command = "${my-xmonad}/bin/xmonad";
      home.file = {
        ".myXmonadBinary" = {
          text = ''
            "${my-xmonad}/bin/xmonad"
          '';
          onChange = ''
            if [[ -v DISPLAY ]] ; then
              echo "Restarting xmonad"
              $DRY_RUN_CMD ${config.home-manager.users.mhuber.xsession.windowManager.command} --restart
            fi
          '';
        };
      };
      services.picom = let
        excludes = [
          "window_type *= 'menu'"
          "class_g = 'firefox' && argb"
          "name ~= 'Zoom Meeting'" "class_g = 'zoom'"
          # "name ~= 'Firefox$'"
          # "focused = 1"
          # # "menu        = { shadow = false; };"
          # "dropdown_menu = { shadow = false; };"
          # "popup_menu    = { shadow = false; };"
          # "utility       = { shadow = false; };"
        ];
        in {
        enable = true;
        blur = true;
        blurExclude = excludes;
        fade = true;
        fadeExclude = excludes;
        shadow = true;
        shadowExclude = excludes;
        vSync = true;
        inactiveDim = "0.2";
        menuOpacity = "0.8";
        extraOptions = ''
          unredir-if-possible = true;
          dbe = true;
          inactive-opacity-override = false;
          # https://www.reddit.com/r/xmonad/comments/ih2scc/picom_opacity_not_working_in_xmonad/
          mark-ovredir-focused = false;
          use-ewmh-active-win = true;
        '';
      };
    };
    environment.variables = {
      XSECURELOCK_BLANK_TIMEOUT = "-1";
      XSECURELOCK_COMPOSITE_OBSCURER = "0";
      # XSECURELOCK_NO_COMPOSITE = "1";
    };


    # system.activationScripts.cleanupXmonadState = "rm $HOME/.xmonad/xmonad.state || true";

    services = {
      xserver = {
        displayManager.defaultSession = "none+myXmonad";
        windowManager = {
          session = [{
            name = "myXmonad";
            start = ''
              exec &> >(tee -a /tmp/myXmonad.log)
              echo -e "\n\n$(date)\n\n"
              ${my-xmonad}/bin/xmonad &
              waitPID=$!
            '';
          }];
        };

        desktopManager.xterm.enable = false;
      };
    };
  };
}
