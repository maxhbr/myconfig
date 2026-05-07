# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:

let
  forgejoDomain = "forgejo.${config.networking.hostName}.wg0.maxhbr.local";
in
{
  config = {
    services.openssh.enable = true;

    services.forgejo = {
      enable = true;
      database.type = "postgres";

      lfs.enable = true;

      settings = {
        server = {
          DOMAIN = forgejoDomain;
          ROOT_URL = "http://${forgejoDomain}:3000/";
          HTTP_PORT = 3000;
          SSH_PORT = 22;
        };

        service = {
          DISABLE_REGISTRATION = true;
        };
      };
    };

    networking.firewall.interfaces."wg0".allowedTCPPorts =
      lib.optionals config.services.forgejo.enable
        [
          3000
        ];
  };
}
