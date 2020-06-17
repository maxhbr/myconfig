# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:

{
  config = {
    home-manager.users.mhuber = {
      services.syncthing.enable = true;
    };
    # services.syncthing = {
    #   enable = true;
    #   user = "mhuber";
    #   group = "mhuber";
    #   dataDir = "/home/mhuber";
    #   openDefaultPorts = false;
    # };
  };
}
