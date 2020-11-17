{ config, lib, pkgs, ... }:
let
  user = config.myconfig.user;
in {
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        xclip
      ];

      programs.fish.functions = {
        xclipToX = "${pkgs.xclip}/bin/xclip (${pkgs.xclip}/bin/xclip -selection c -o | psub)";
        xclipToCtrl = "${pkgs.xclip}/bin/xclip -selection c (${pkgs.xclip}/bin/xclip -o | psub)";
      };
    };

    environment.interactiveShellInit = ''
      xclipToX() {
        ${pkgs.xclip}/bin/xclip <(${pkgs.xclip}/bin/xclip -selection c -o)
      }

      xclipToCtrl() {
        ${pkgs.xclip}/bin/xclip -selection c <(${pkgs.xclip}/bin/xclip -o)
      }
    '';
  };
}
