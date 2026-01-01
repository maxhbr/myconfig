# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    desktop.imagework.enable = mkEnableOption "imagework";
    desktop.imagework.myphoto.enable = mkEnableOption "myphoto" // {
      default = true;
    };
  };
  config = (
    lib.mkIf cfg.desktop.imagework.enable {
      home-manager.sharedModules = [
        {
          home.packages = [
          ]
          ++ (with pkgs; [
            gphoto2
            gphoto2fs

            # rawtherapee
            # gthumb
            # krita
            # inkscape
          ]);
          myconfig.desktop.imagework.geeqie.enable = true;
          myconfig.desktop.imagework.darktable.enable = true;
          myconfig.desktop.imagework.digikam.enable = true;
          myconfig.desktop.imagework.gimp.enable = true;
          myconfig.desktop.imagework.sigal.enable = true;
        }
        ./home-manager.gimp.nix
        ./home-manager.darktable.nix
        ./home-manager.digikam.nix
        ./home-manager.geeqie.nix
        ./home-manager.sigal.nix
      ]
      ++ lib.optionals cfg.desktop.imagework.myphoto.enable [ inputs.myphoto.homeManagerModules.myphoto ];
    }
  );
}
