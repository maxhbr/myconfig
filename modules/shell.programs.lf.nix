# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, myconfig, ... }: {
  config = {
    home-manager.sharedModules = [{
      programs.lf = {
        enable = true;
        settings = { };
        keybindings = {
          gh = "cd ~";
          # D = "trash";
          i = "$less $f";
          U = "!du -chs *|sort -h|less";
          # gg = null;
        };
        cmdKeybindings = { "<c-g>" = "cmd-escape"; };
        previewer = {
          source = pkgs.writeShellScript "pv.sh" ''
            #!/bin/sh
            case "''${1,,}" in
                *.tar*) tar tf "$1";;
                *.zip) ${pkgs.p7zip}/bin/7z l "$1";;
                *.rar) ${pkgs.p7zip}/bin/7z l "$1";;
                *.7z) ${pkgs.p7zip}/bin/7z l "$1";;
                *.dll|*.exe|*.ttf|*.woff|*.otf|*eot) ${pkgs.exiftool}/bin/exiftool "$1";;
                *) ${pkgs.pistol}/bin/pistol "$1";;
            esac
          '';
          # *.pdf) pdftotext "$1" -;;
          keybinding = "i";
        };
        extraConfig = ''
          map <enter> shell
          map o &mimeopen $f
          map O $mimeopen --ask $f
          cmd open ''${{
              test -L $f && f=$(readlink -f $f)
              case $(file --mime-type $f -b) in
                  text/*) $EDITOR $fx;;
                  *) for f in $fx; do setsid $OPENER $f > /dev/null 2> /dev/null & done;;
              esac
          }}
        '';
      };
    }];
    environment = {
      shellAliases = {
        ranger = "lf";
        r = "lf";
      };
    };
  };
}
