# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        abook urlview notmuch
        sxiv
        unstable.astroid
      ];
    };
    environment = {
      systemPackages = with pkgs; [
        offlineimap gnupg
        msmtp
        procmail
        isync
      ];
    };
    # services.offlineimap = {
    #   enable = false;
    #   path = with pkgs; [ notmuch ];
    # };
  };
}
