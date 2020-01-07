# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
{
  config = {
    environment.systemPackages = with pkgs; ([
      meld
      gnumake cmake automake
      cloc
      pass-git-helper
    ] ++ lib.optional config.services.xserver.enable vscode-with-extensions);
  };
}
