# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      oh-my-zsh
    ];
    shells = [
      "${pkgs.zsh}/bin/zsh"
      "/run/current-system/sw/bin/zsh"
    ];
  };

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    ohMyZsh = {
      enable = true;
      plugins = ["git"];
    };
    promptInit = ""; # Clear this to avoid a conflict with oh-my-zsh
  };
}
