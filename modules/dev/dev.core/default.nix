# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev;
  cropLog = with pkgs; writeScriptBin "cropLog.hs" (lib.fileContents ./cropLog.hs);
in
{
  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [
      (self: super: {
        my-meld = pkgs.meld.overrideAttrs (old: {
          postFixup = old.postFixup + ''
            wrapProgram $out/bin/meld --unset WAYLAND_DISPLAY
          '';
        });
      })
    ];
    home-manager.sharedModules = [
      {
        programs.gh.enable = true;
        home.packages =
          with pkgs;
          (
            [
              my-meld
              # diffoscope
              gnumake
              cmake
              automake
              cloc
              pass-git-helper
              jq
              yq
              csvkit
              (writeShellScriptBin "cq" ''
                # Run jq against a CSV file via csvjson.
                # Usage:
                #   cq file.csv '<jq filter>'
                #   cat data.csv | cq '<jq filter>'
                set -euo pipefail
                if [[ $# -ge 1 && -f "$1" ]]; then
                  ${pkgs.csvkit}/bin/csvjson "$1" | ${pkgs.jq}/bin/jq "$${@:2}"
                else
                  ${pkgs.csvkit}/bin/csvjson - | ${pkgs.jq}/bin/jq "$@"
                fi
              '')
              cropLog
              mercurial
              gnuplot
              plantuml
              graphviz
              darcs
            ]
            ++ lib.optional config.myconfig.desktop.enable freeplane
            ++ lib.optional config.services.xserver.wacom.enable xournalpp
          );
      }
    ];
  };
}
