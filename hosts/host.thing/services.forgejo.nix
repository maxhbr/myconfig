# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, ... }:

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
          ROOT_URL = "https://${forgejoDomain}/";
          HTTP_PORT = 3000;
          SSH_PORT = 22;
        };

        service.DISABLE_REGISTRATION = true;
      };
    };

    # Users are created automatically at boot; passwords come from
    # myconfig.secrets (provisioned in the priv/ repository).
    myconfig.forgejo.server = {
      enable = true;
      users = {
        maxhbr = {
          admin = true;
          email = "root@localhost";
          passwordSecret = "forgejo-admin-password";
          gitSshKeys = "mhuber";
        };
        "hermes-agent" = { };
      };
    };

    networking.firewall.interfaces."wg0".allowedTCPPorts =
      lib.optionals config.services.forgejo.enable
        [
          3000
        ];
  };
}
