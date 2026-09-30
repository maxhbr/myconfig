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
  cq = pkgs.writeShellScriptBin "cq" ''
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
  '';
  # The CLI tools of this module that are also useful inside the mysbx
  # sandbox. Left out: the GUI tools, pass-git-helper (needs the host's
  # pass store) and cropLog (fetches ghc via nix-shell at run time).
  sandboxCliTools = with pkgs; [
    gh
    gnumake
    just
    cmake
    automake
    cloc
    jq
    yq
    csvkit
    cq
    mercurial
    gnuplot
    plantuml
    graphviz
    darcs
  ];
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
    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = sandboxCliTools;
      # mkOptionDefault merges with the option's default list instead of
      # replacing it.
      podman.imagePackages = lib.mkOptionDefault sandboxCliTools;
    };
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
              just
              cmake
              automake
              cloc
              pass-git-helper
              jq
              yq
              csvkit
              cq
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
