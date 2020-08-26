# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  syncthingTunnel = with pkgs; pkgs.writeShellScriptBin "syncthingTunnel" ''
set -x
exec \
    ssh -N \
        -R ''${2:-9384}:localhost:8384 \
        -L ''${2:-9384}:localhost:8384 \
        "''${1}"
  '';
in
{
  config =
    { services.syncthing =
        { enable = true;
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
    home-manager.users.mhuber =
      { home.packages =
          [ syncthingTunnel
          ];
      };
  };
}
