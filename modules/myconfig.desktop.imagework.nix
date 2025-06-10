# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, inputs, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.imagework.enable = mkEnableOption "imagework";
    desktop.imagework.myphoto.enable = mkEnableOption "myphoto" // {
      default = true;
    };
  };
  config = (lib.mkIf cfg.desktop.imagework.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          gphoto2
          gphoto2fs
          gimp # -with-plugins
          darktable
          # ansel # A darktable fork minus the bloat plus some design vision
          geeqie
          gphoto2
          gphoto2fs

          # rawtherapee
          # gthumb
          # krita
          # inkscape
          sigal # static website generator
        ];
        myconfig.persistence.files = [
          ".config/darktable/darktablerc"
          ".config/darktable/shortcutsrc"
        ];
      }
    ] ++ lib.optionals cfg.desktop.imagework.myphoto.enable [
      inputs.myphoto.homeManagerModules.myphoto
    ];
  });
}
