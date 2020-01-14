# Copyright 2019-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        vagrant
      ];
      home.file = {
        ".vagrant.d/Vagrantfile".source = ./vagrant.d/Vagrantfile;
      };
    };
    virtualisation.virtualbox.host.enable = true;
  };
}
