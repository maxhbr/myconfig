# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig.dev;
  cropLog = with pkgs;
    writeScriptBin "cropLog.hs" (lib.fileContents ./cropLog.hs);
in {
  config = lib.mkIf cfg.enable {
    home-manager.users.mhuber = {
      home.packages = with pkgs;
        ([
          meld
          gnumake
          cmake
          automake
          cloc
          pass-git-helper
          jq
          cropLog
          mercurialFull
          plantuml
          graphviz
        ]
        ++ lib.optional config.services.xserver.enable
          vscode-with-extensions
        ++ lib.optional config.services.xserver.enable
          unstable.freeplane
        ++ lib.optional config.services.xserver.enable
          xournalpp
        );
    };
  };
}
