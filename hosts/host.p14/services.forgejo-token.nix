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
      symlink = false;
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
      # Fire-and-forget: no retries, no blocking, graceful failure.

      PASSWORD=$(tr -d '\n' < ${config.myconfig.secrets.forgejo-admin-password.dest} 2>/dev/null || true)

      if [ -z "$PASSWORD" ]; then
        echo "No password available, skipping token creation"
        exit 0
      fi

      response=$(
        curl \
          --silent \
          --max-time 5 \
          --user "${forgejoAdminUser}:$PASSWORD" \
          --header 'Content-Type: application/json' \
          --request POST \
          --data '{
            "name": "hermes-agent",
            "expires_at": 4102444800
          }' \
          "${forgejoApi}/admin/users/${hermesAgentUser}/tokens" \
          2>/dev/null || echo ""
      )

      token=$(echo "$response" | jq -r '.sha1 // empty' 2>/dev/null || true)

      if [ -n "$token" ]; then
        echo "$token" > /run/forgejo-hermes-agent-token
        chmod 640 /run/forgejo-hermes-agent-token
        echo "Created API token for ${hermesAgentUser}"
      else
        echo "Skipping token creation (Forgejo unreachable or request failed)"
      fi

      exit 0
    '';
  };
}
