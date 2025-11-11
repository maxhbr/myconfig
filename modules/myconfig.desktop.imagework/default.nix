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
  mk_sigal_gallery = pkgs.writeShellApplication {
    name = "mk_sigal_gallery";
    runtimeInputs = with pkgs; [ sigal ];
    text = builtins.readFile ./mk_sigal_gallery.sh;
  };
in
{
  options.myconfig = with lib; {
    desktop.imagework.enable = mkEnableOption "imagework";
    desktop.imagework.myphoto.enable = mkEnableOption "myphoto" // {
      default = true;
    };
    desktop.imagework.sigal.enable = mkEnableOption "sigal";
  };
  config = (
    lib.mkIf cfg.desktop.imagework.enable {
      home-manager.sharedModules = [
        {
          home.packages =
            with pkgs;
            [
              gphoto2
              gphoto2fs
              gimp3-with-plugins
              darktable
              (writeShellScriptBin "darktable-here" ''
                here=$(pwd)
                arg="''${1:-$here}"
                darktable_dir="$here/../darktable_here"

                mkdir -p "$darktable_dir"

                exec ${darktable}/bin/darktable \
                    `#--configdir "$darktable_dir/config"` \
                    --cachedir "$darktable_dir/cache" \
                    --library "$darktable_dir/library.db" \
                    `#--datadir "$darktable_dir/data"` \
                    --tmpdir "$darktable_dir/tmp" \
                    --dumpdir "$darktable_dir" \
                    "$arg"
              '')
              # ansel # A darktable fork minus the bloat plus some design vision
              geeqie
              gphoto2
              gphoto2fs

              # rawtherapee
              # gthumb
              # krita
              # inkscape
            ]
            ++ lib.optionals cfg.desktop.imagework.sigal.enable [
              sigal
              mk_sigal_gallery
            ];
          myconfig.persistence.files = [
            ".config/darktable/darktablerc"
            ".config/darktable/shortcutsrc"
            ".config/geeqie/geeqierc.xml"
          ];
        }
      ]
      ++ lib.optionals cfg.desktop.imagework.myphoto.enable [ inputs.myphoto.homeManagerModules.myphoto ];
      myconfig.desktop.wayland.launcherCommands = [
        "geeqie"
        "darktable"
        "gimp"
      ];
    }
  );
}
