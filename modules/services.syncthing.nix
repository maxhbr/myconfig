# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  user = "mhuber";
  syncthingTunnel = with pkgs;
    pkgs.writeShellScriptBin "syncthingTunnel" ''
      set -x
      exec \
          ssh -N \
              -R ''${2:-9384}:localhost:8384 \
              -L ''${2:-9384}:localhost:8384 \
              "''${1}"
        '';
in {
  config = lib.mkIf config.services.syncthing.enable {
    services.syncthing = {
      overrideDevices = true;
      overrideFolders = true;
      user = "${user}";
      group = "${user}";
      dataDir = "/home/${user}/syncthing";
      configDir = "/home/${user}/syncthing/.config/syncthing";
      openDefaultPorts = true;
    };
    home-manager.users."${user}" = { home.packages = [ syncthingTunnel ]; };
  };
}
