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
