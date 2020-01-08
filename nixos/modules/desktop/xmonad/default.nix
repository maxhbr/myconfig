# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  my-xmonad = pkgs.haskellPackages.callPackage ./myXmonad {
    inherit pkgs;
  };
in {
  imports = [
    ../X.nix
  ];

  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        my-xmonad
        dzen2
        rxvt_unicode_with-plugins rxvt_unicode.terminfo
      ];
      xsession.windowManager.command = "${my-xmonad}/bin/xmonad";
    };

    # system.activationScripts.cleanupXmonadState = "rm $HOME/.xmonad/xmonad.state || true";

    services = {
      xserver = {
        windowManager = {
          default = "myXmonad";
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

        desktopManager = {
          xterm.enable = false;
          default = "none";
        };
      };
      urxvtd = {
        enable = true;
        # users = [ "mhuber" ];
        # urxvtPackage = pkgs.rxvt_unicode_with-plugins;
      };
    };
  };
}
