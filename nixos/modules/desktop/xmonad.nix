# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./X.nix
  ];

  options = {
  };

  config = {
    environment.systemPackages = with pkgs; [
      pkgs.my-xmonad
      dzen2
      rxvt_unicode_with-plugins rxvt_unicode.terminfo
    ];

    # system.activationScripts.cleanupXmonadState = "rm $HOME/.xmonad/xmonad.state || true";

    services = {
      xserver = {
        windowManager = {
          # xmonad = {
          #   enable = true;
          #   enableContribAndExtras = true;
          #   # extraPackages = haskellPackages: [ pkgs.myconfig.xmonad-config ];
          # };
          default = "myXmonad";
          session = [{
            name = "myXmonad";
            start = ''
              exec &> >(tee -a /tmp/myXmonad.log)
              echo -e "\n\n$(date)\n\n"
              ${pkgs.my-xmonad}/bin/xmonad &
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
