{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig.desktop.imagework.gimp;
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
in
{
  options.myconfig.desktop.imagework.gimp.enable = lib.mkEnableOption "gimp";
  config = lib.mkIf cfg.enable {
    home.packages = [
      mygimp
      mygimp-export
    ];
    myconfig.desktop.wayland.launcherCommands = [
      "gimp"
    ];
  };
}
