# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  imports = [ ../mail.common ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        mu
        gnome3.gnome-keyring # necessary for mu4e?
      ];
    };
  };
}
