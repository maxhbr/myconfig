# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# see also:
#  - https://gist.github.com/datakurre/cd29a985351e6b8c9bbc04532e5f9df0
{ pkgs, config, ... }: {
  config = {
    users = {
      extraUsers = {
        kiosk = {
          isNormalUser = true;
          description = "Kiosk User";
          home = "/home/kiosk";
          group = "kiosk";
          uid = 1900;
          extraGroups = [ "audio" "input" "networkmanager" "video" ];
          openssh.authorizedKeys.keys =
            config.users.extraUsers.mhuber.openssh.authorizedKeys.keys;
        };
        mhuber.extraGroups = [ "kiosk" ];
      };
      extraGroups.kiosk.gid = 1900;
    };
  };
}
