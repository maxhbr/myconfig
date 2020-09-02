# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  cropLog = with pkgs; writeScriptBin "cropLog.hs" (lib.fileContents ./cropLog.hs);
  freeplane = pkgs.callPackage ../../pkgs/freeplane {};
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; ([
        meld
        gnumake cmake automake
        cloc
        pass-git-helper
        jq
        cropLog
        mercurialFull
        freeplane xournalpp
        plantuml graphviz
      ] ++ lib.optional config.services.xserver.enable vscode-with-extensions);
    };
  };
}
