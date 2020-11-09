# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }: let
  user = config.myconfig.user;
in {
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        abook
        urlview
        notmuch
        sxiv
        unstable.astroid
      ];
    };
    environment = {
      systemPackages = with pkgs; [ offlineimap gnupg msmtp procmail isync ];
    };
    # services.offlineimap = {
    #   enable = false;
    #   path = with pkgs; [ notmuch ];
    # };
  };
}
