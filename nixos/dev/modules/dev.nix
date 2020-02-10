# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  cropLog = with pkgs; writeScriptBin "cropLog.hs" (lib.fileContents ./cropLog.hs);
in {
  config = {
    environment.systemPackages = with pkgs; ([
      meld
      gnumake cmake automake
      cloc
      pass-git-helper
      jq
      cropLog
    ] ++ lib.optional config.services.xserver.enable vscode-with-extensions);
  };
}
