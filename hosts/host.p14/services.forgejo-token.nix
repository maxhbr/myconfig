# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:

let
  forgejoHost = myconfig.metadatalib.getWgIp "thing";

  forgejoPort = 3000;
  forgejoApi = "http://${forgejoHost}:${toString forgejoPort}/api/v1";

  forgejoAdminUser = "maxhbr";
  hermesAgentUser = "hermes-agent";
in
{
  myconfig.secrets = {
    forgejo-admin-password = {
      dest = "/run/forgejo-admin-password";
      owner = "root";
      group = "root";
    };
  };

  systemd.services.forgejo-create-hermes-agent-token = {
    description = "Create API token for Forgejo hermes-agent user";
    wantedBy = [ "multi-user.target" ];
    after = [
      "network-online.target"
      "forgejo-admin-password-key.service"
    ];
    wants = [ "network-online.target" ];

    path = [
      pkgs.curl
      pkgs.jq
    ];

    serviceConfig = {
      Type = "oneshot";
      User = "root";
      Group = "root";
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

      response=$(
        curl \
          --silent \
          --max-time 10 \
          --user "${forgejoAdminUser}:$(tr -d '\n' < ${config.myconfig.secrets.forgejo-admin-password.dest})" \
          --header 'Content-Type: application/json' \
          --request POST \
          --data '{
            "name": "hermes-agent",
            "expires_at": 4102444800
          }' \
          "${forgejoApi}/admin/users/${hermesAgentUser}/tokens"
      )

      token=$(echo "$response" | jq -r '.sha1 // empty')

      if [ -n "$token" ]; then
        echo "$token" > /run/forgejo-hermes-agent-token
        chmod 640 /run/forgejo-hermes-agent-token
        echo "Created API token for ${hermesAgentUser}"
      else
        echo "Failed to create API token for ${hermesAgentUser}"
        echo "$response"
        exit 1
      fi
    '';
  };
}
