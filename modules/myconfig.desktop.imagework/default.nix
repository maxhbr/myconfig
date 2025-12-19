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

  mygimp = pkgs.gimp3-with-plugins;
  mygimp-export = pkgs.writeShellApplication {
    name = "gimp-export";
    runtimeInputs = with pkgs; [ mygimp ];
    text = ''
      #/bin/sh
      set -euo pipefail

      getScript() {
          local in="$1"
          local png="$2"
          local jpg="''${3:-}"
          cat <<EOF
      (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE "$in" "$in")))
            (merged (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE))))
       (file-png-export
         #:run-mode       RUN-NONINTERACTIVE
         #:image          image
         #:file           "$png"
         #:interlaced     FALSE
         #:compression    9
         #:bkgd           FALSE
         #:offs           FALSE
         #:phys           FALSE
         #:time           FALSE
         #:save-transparent TRUE)
      EOF
         if [[ ! -z "$jpg" ]]; then
              cat <<EOF
        (let*
          (
            (width  (car (gimp-image-get-width  image)))
            (height (car (gimp-image-get-height image)))
            (max-width  3840)
            (max-height 2160)
            (scale-x (/ max-width  width))
            (scale-y (/ max-height height))
            ;; choose the smaller scale so both sides fit, but never upscale
            (scale-factor (min 1.0 (min scale-x scale-y)))
          )

          (when (< scale-factor 1.0)
            (gimp-image-scale
              image
              (round (* scale-factor width))
              (round (* scale-factor height))))
        )

        (file-jpeg-export RUN-NONINTERACTIVE image "$jpg" -1 0.93)
      EOF
          fi
          cat <<EOF
        (gimp-image-delete image)
      )
      EOF
      }

      for in_file in "$@"; do
          in="$in_file"
          base="''${in%.xcf}"
          png="''${base}.png"
          jpg="''${base}.4k.jpg"

          gimp-console \
            -n -i \
            --batch-interpreter=plug-in-script-fu-eval \
            -b "$(getScript "$in" "$png" "$jpg")" \
            --quit
      done
    '';
  };

  mydarktable = pkgs.darktable;
  mydarktable-here = (
    pkgs.writeShellScriptBin "darktable-here" ''
      here=$(pwd)
      arg="''${1:-$here}"

      # Search for existing darktable_here directory
      darktable_dir=""
      search_dir="$here"
      while [[ "$search_dir" != "/" ]]; do
        if [[ -d "$search_dir/darktable_here" ]]; then
          darktable_dir="$search_dir/darktable_here"
          break
        fi
        search_dir=$(dirname "$search_dir")
      done

      # Fallback: create new directory at ../darktable_here
      if [[ -z "$darktable_dir" ]]; then
        darktable_dir="$here/../darktable_here"
      fi

      mkdir -p "$darktable_dir"

      exec ${mydarktable}/bin/darktable \
          `#--configdir "$darktable_dir/config"` \
          --cachedir "$darktable_dir/cache" \
          --library "$darktable_dir/library.db" \
          `#--datadir "$darktable_dir/data"` \
          --tmpdir "$darktable_dir/tmp" \
          --dumpdir "$darktable_dir" \
          "$arg"
    ''
  );

  mydigikam = pkgs.digikam;
  mydigikam-here = (
    pkgs.writeShellScriptBin "digikam-here" ''
      here=$(pwd)
      arg="''${1:-$here}"

      # Search for existing digikam_here directory
      digikam_dir=""
      search_dir="$here"
      while [[ "$search_dir" != "/" ]]; do
        if [[ -d "$search_dir/digikam_here" ]]; then
          digikam_dir="$search_dir/digikam_here"
          break
        fi
        search_dir=$(dirname "$search_dir")
      done

      # Fallback: create new directory at ../digikam_here
      if [[ -z "$digikam_dir" ]]; then
        digikam_dir="$here/../digikam_here"
      fi

      mkdir -p "$digikam_dir"

      export XDG_CONFIG_HOME="$digikam_dir/config"
      export XDG_DATA_HOME="$digikam_dir/data"
      export XDG_CACHE_HOME="$digikam_dir/cache"

      mkdir -p "$XDG_CONFIG_HOME" "$XDG_DATA_HOME" "$XDG_CACHE_HOME"

      exec ${mydigikam}/bin/digikam "$arg"
    ''
  );
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
          home.packages = [
            mygimp
            mygimp-export
            mydarktable
            mydarktable-here
            mydigikam
            mydigikam-here
          ]
          ++ (
            with pkgs;
            [
              gphoto2
              gphoto2fs
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
            ]
          );
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
        "digikam"
        "gimp"
      ];
    }
  );
}
