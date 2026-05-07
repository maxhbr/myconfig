# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:

let
  forgejoDomain = "forgejo.${config.networking.hostName}.wg0.maxhbr.local";

  forgejoUser = "maxhbr";
  nixosUser = "mhuber";

  hermesAgentUser = "hermes-agent";

  forgejoPort = config.services.forgejo.settings.server.HTTP_PORT;
  forgejoApi = "http://127.0.0.1:${toString forgejoPort}/api/v1";

  sshKeys = config.users.users.${nixosUser}.openssh.authorizedKeys.keys;

  addKeyCommands = lib.concatMapStringsSep "\n" (
    key:
    let
      title = "nixos-${nixosUser}-${builtins.substring 0 12 (builtins.hashString "sha256" key)}";
    in
    ''
      key_json=$(jq -cn \
        --arg title ${lib.escapeShellArg title} \
        --arg key ${lib.escapeShellArg key} \
        '{title: $title, key: $key, read_only: false}')

      response_file=$(mktemp)
      trap 'rm -f "$response_file"' RETURN

      status=$(
        curl \
          --silent \
          --show-error \
          --max-time 10 \
          --output "$response_file" \
          --write-out '%{http_code}' \
          --user "${forgejoUser}:$(tr -d '\n' < ${config.myconfig.secrets.forgejo-admin-password.dest})" \
          --header 'Content-Type: application/json' \
          --request POST \
          --data "$key_json" \
          "${forgejoApi}/admin/users/${forgejoUser}/keys"
      )

      case "$status" in
        201)
          echo "Added Forgejo SSH key ${title}"
          ;;
        422)
          echo "Forgejo SSH key ${title} already exists or is not accepted; ignoring"
          ;;
        *)
          echo "Unexpected status while adding Forgejo SSH key ${title}: $status"
          cat "$response_file" || true
          rm -f "$response_file"
          exit 1
          ;;
      esac

      rm -f "$response_file"
    ''
  ) sshKeys;
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

    myconfig.secrets = {
      forgejo-admin-password = {
        dest = "/run/forgejo-admin-password";
        owner = "forgejo";
        group = "forgejo";
      };

      forgejo-hermes-agent-password = {
        dest = "/run/forgejo-hermes-agent-password";
        owner = "forgejo";
        group = "forgejo";
      };
    };

    systemd.services.forgejo = {
      serviceConfig.After = [
        "forgejo-admin-password-key.service"
        "forgejo-hermes-agent-password-key.service"
      ];

      preStart =
        let
          cfg = config.services.forgejo;
          adminCmd = "${lib.getExe cfg.package} admin user";
          pwd = config.myconfig.secrets.forgejo-admin-password;
          haPwd = config.myconfig.secrets.forgejo-hermes-agent-password;
        in
        ''
          ${adminCmd} create \
            --admin \
            --email "root@localhost" \
            --username ${forgejoUser} \
            --password "$(tr -d '\n' < ${pwd.dest})" \
            || true

          ${adminCmd} create \
            --email "hermes-agent@localhost" \
            --username ${hermesAgentUser} \
            --password "$(tr -d '\n' < ${haPwd.dest})" \
            || true
        '';
    };

    systemd.services.forgejo-add-admin-ssh-keys = {
      description = "Add NixOS user SSH keys to Forgejo admin user";
      wantedBy = [ "multi-user.target" ];
      after = [
        "forgejo.service"
        "forgejo-admin-password-key.service"
      ];
      wants = [ "forgejo.service" ];

      path = [
        pkgs.curl
        pkgs.jq
      ];

      serviceConfig = {
        Type = "oneshot";
        User = "forgejo";
        Group = "forgejo";
      };

      script = ''
        set -euo pipefail

        for i in $(seq 1 60); do
          if curl --silent --fail --max-time 2 \
            "${forgejoApi}/version" \
            >/dev/null; then
            break
          fi

          if [ "$i" = 60 ]; then
            echo "Forgejo API did not become ready"
            exit 1
          fi

          sleep 1
        done

        ${addKeyCommands}
      '';
    };

    networking.firewall.interfaces."wg0".allowedTCPPorts =
      lib.optionals config.services.forgejo.enable
        [
          3000
        ];
  };
}
