# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:

{
  config = {
    services.syncthing = {
      enable = true;
      user = "mhuber";
      group = "mhuber";
      dataDir = "/home/mhuber/syncthing";
      openDefaultPorts = false;
    };
  };
}
