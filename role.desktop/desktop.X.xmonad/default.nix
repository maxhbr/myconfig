# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  user = config.myconfig.user;
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
    home-manager.users."${user}" = {
      imports = [ ./picom.hm.nix ];
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
              $DRY_RUN_CMD ${
                config.home-manager.users."${user}".xsession.windowManager.command
              } --restart
            fi
          '';
        };
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
