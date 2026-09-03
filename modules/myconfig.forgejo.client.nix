# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Forgejo client-side provisioning for agent hosts: for every entry in
# `myconfig.forgejo.client.tokens` (keyed by Forgejo user name) this module
# - declares the matching `myconfig.secrets.<passwordSecret>` entry (owned by
#   root; the secret itself must be provisioned in the `priv/` repository),
# - creates a oneshot `forgejo-create-<user>-token` systemd service that
#   exchanges the password for a long-lived Forgejo API token at boot
#   (fire-and-forget: a missing or unreachable Forgejo never blocks boot) and
#   writes it to `/run/forgejo-<user>-token` for the agent to use.
{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.myconfig.forgejo.client;

  forgejoApi = "${cfg.apiBase}/api/v1";

  tokenScript =
    name: token:
    let
      secret = config.myconfig.secrets.${token.passwordSecret};
    in
    ''
      # Fire-and-forget: no retries, no blocking, graceful failure.

      PASSWORD=$(tr -d '\n' < ${secret.dest} 2>/dev/null || true)

      if [ -z "$PASSWORD" ]; then
        echo "No password available, skipping token creation"
        exit 0
      fi

      response=$(
        curl \
          --silent \
          --max-time 5 \
          --user "${name}:$PASSWORD" \
          --header 'Content-Type: application/json' \
          --request POST \
          --data '{
            "name": "${name}",
            "expires_at": ${toString token.expiresAt}
          }' \
          "${forgejoApi}/users/me/tokens" \
          2>/dev/null || echo ""
      )

      token=$(echo "$response" | jq -r '.sha1 // empty' 2>/dev/null || true)

      if [ -n "$token" ]; then
        echo "$token" > ${token.tokenDest}
        chmod 640 ${token.tokenDest}
        echo "Created API token for ${name}"
      else
        echo "Skipping token creation (Forgejo unreachable or request failed)"
      fi

      exit 0
    '';
in
{
  options.myconfig.forgejo.client = with lib; {
    enable = mkEnableOption "myconfig.forgejo.client";

    apiBase = mkOption {
      type = types.str;
      description = "Base URL of the Forgejo instance (scheme, host and port), e.g. `http://thing.wg0.maxhbr.local:3000`.";
    };

    tokens = mkOption {
      type = types.attrsOf (
        types.submodule (
          { name, ... }: {
            options = {
              passwordSecret = mkOption {
                type = types.str;
                default = "forgejo-${name}-password";
                defaultText = literalExpression ''"forgejo-${name}-password"'';
                description = "Name of the `myconfig.secrets` entry holding the user's password. The secret is declared automatically (dest `/run/<passwordSecret>`, owned by root, no symlink) and must be provisioned in the `priv/` repository.";
              };

              tokenDest = mkOption {
                type = types.str;
                default = "/run/forgejo-${name}-token";
                defaultText = literalExpression ''"/run/forgejo-${name}-token"'';
                description = "Path where the created API token is written.";
              };

              expiresAt = mkOption {
                type = types.int;
                default = 4102444800;
                defaultText = literalExpression "4102444800";
                description = "Token expiry as a Unix timestamp (default: 2100-01-01 UTC).";
              };
            };
          }
        )
      );
      default = { };
      description = "Forgejo users for which an API token is created at boot, keyed by Forgejo user name.";
    };
  };

  config = lib.mkIf cfg.enable {
    myconfig.secrets = lib.listToAttrs (
      lib.mapAttrsToList (
        _name: token:
        lib.nameValuePair token.passwordSecret {
          dest = "/run/${token.passwordSecret}";
          owner = "root";
          group = "root";
          symlink = false;
        }
      ) cfg.tokens
    );

    systemd.services = lib.mapAttrs' (
      name: token:
      lib.nameValuePair "forgejo-create-${name}-token" {
        description = "Create API token for Forgejo ${name} user";
        wantedBy = [ "multi-user.target" ];
        after = [
          "network-online.target"
          "${token.passwordSecret}-key.service"
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

        script = tokenScript name token;
      }
    ) cfg.tokens;
  };
}
