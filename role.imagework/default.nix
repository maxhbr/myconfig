# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let user = config.myconfig.user;
in {
  imports = [ ../role.desktop ./wacom.nix ./exfat.nix ];
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        gphoto2
        gphoto2fs

        gimp # -with-plugins
        rawtherapee
        unstable.darktable
        unstable.geeqie
        unstable.gthumb
        krita
        # inkscape

        blender
        librecad # 2D
        nixos-2003-small.freecad # 3D
      ];
    };
    environment.interactiveShellInit = ''
      gimp() {
        command gimp "$@" &disown
      }
      darktable() {
        command darktable "$@" &disown
        }
      gthumb() {
        command gthumb "$@" &disown
      }
          '';
  };
}
