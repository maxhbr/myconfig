# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        offlineimap msmtp gnupg abook urlview notmuch
        sxiv
        procmail
        unstable.astroid
        isync
      ];
    };
    # services.offlineimap = {
    #   enable = false;
    #   path = with pkgs; [ notmuch ];
    # };
  };
}
