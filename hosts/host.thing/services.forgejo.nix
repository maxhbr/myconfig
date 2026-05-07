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
          ROOT_URL = "https://${forgejoDomain}/";
          HTTP_PORT = 3000;
          SSH_PORT = 22;
        };

        service = {
          DISABLE_REGISTRATION = true;
        };
      };
    };

    systemd.services.forgejo = {
      serviceConfig.After = [ "forgejo-admin-password-key.service" ];

      preStart =
        let
          cfg = config.services.forgejo;
          adminCmd = "${lib.getExe cfg.package} admin user";
          pwd = config.myconfig.secrets.forgejo-admin-password;
          user = "maxhbr";
        in
        ''
          ${adminCmd} create --admin --email "root@localhost" --username ${user} --password "$(tr -d '\n' < ${pwd.dest})" || true
        '';
    };

    myconfig.secrets = {
      forgejo-admin-password = {
        dest = "/run/forgejo-admin-password";
        owner = "forgejo";
        group = "forgejo";
      };
    };

    networking.firewall.interfaces."wg0".allowedTCPPorts =
      lib.optionals config.services.forgejo.enable
        [
          3000
        ];
  };
}
