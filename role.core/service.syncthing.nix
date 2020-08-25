# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:

{
  config =
    { services.syncthing = {
        enable = true;
        declarative =
          { cert = "/etc/syncthing/cert.pem";
            key = "/etc/syncthing/key.pem";
            overrideDevices = true;
            overrideFolders = true;
          };
        user = "mhuber";
        group = "mhuber";
        dataDir = "/home/mhuber/syncthing";
        configDir = "/home/mhuber/syncthing/.config/syncthing";
        openDefaultPorts = true;
      };
  };
}
