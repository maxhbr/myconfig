# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      neomutt
      offlineimap msmtp gnupg abook urlview notmuch
      sxiv
      procmail
      unstable.astroid
      mu isync
      gnome3.gnome-keyring # necessary for mu4e?
    ];

    # services.offlineimap = {
    #   enable = false;
    #   path = with pkgs; [ notmuch ];
    # };
  };
}
