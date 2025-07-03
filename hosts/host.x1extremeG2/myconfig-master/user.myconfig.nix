# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    # users = {
    #   mutableUsers = false;
    #   extraUsers = {
    #     myconfig = {
    #       isNormalUser = true;
    #       group = "myconfig";
    #       uid = 1001;
    #       home = "/home/myconfig";
    #       createHome = true;
    #       shell = "/run/current-system/sw/bin/zsh";
    #       # subUidRanges =
    #       #   [ { startGid = 1000; count = 1; }
    #       #   ];
    #       packages = with pkgs; [
    #         nix
    #         ncurses
    #         git
    #         wget
    #         glibcLocales
    #         openssl
    #         nixops
    #       ];
    #     };
    #   };
    #   extraGroups.myconfig.gid = 1001;
    # };
  };
}
