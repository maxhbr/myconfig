# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Forgejo client-side provisioning for agent hosts: for every entry in
# `myconfig.forgejo.client.tokens` (keyed by Forgejo user name) this module
# - declares the matching `myconfig.secrets.<passwordSecret>` entry (owned by
#   root; the secret itself must be provisioned in the `priv/` repository),
# - creates a oneshot `forgejo-create-<user>-token` systemd service that
#   rotates/recreates the API token at boot and writes it to
#   `/run/forgejo-<user>-token` for the agent to use.
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
      scopesJson = builtins.toJSON token.scopes;
      tokenDest = lib.escapeShellArg token.tokenDest;
    in
    ''
      set -euo pipefail

      user_password=$(tr -d '\n' < ${secret.dest} 2>/dev/null || true)

      if [ -z "$user_password" ]; then
        echo "No password available, failing token creation"
        exit 1
      fi

      token_dest=${tokenDest}
      token_dir=$(dirname "$token_dest")
      rm -f "$token_dest"

      auth_file=""
      response_file=""
      tmp_token_file=""

      cleanup() {
        if [ -n "$auth_file" ]; then rm -f "$auth_file"; fi
        if [ -n "$response_file" ]; then rm -f "$response_file"; fi
        if [ -n "$tmp_token_file" ]; then rm -f "$tmp_token_file"; fi
      }

      trap cleanup EXIT

      auth_file=$(mktemp)
      chmod 600 "$auth_file"
      escaped_user_name=$(printf '%s' "${name}" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')
      escaped_user_password=$(printf '%s' "$user_password" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')
      printf 'user = "%s:%s"\n' "$escaped_user_name" "$escaped_user_password" > "$auth_file"

      delete_status=$(
        curl \
          --silent \
          --max-time 5 \
          --config "$auth_file" \
          --request DELETE \
          --output /dev/null \
          --write-out '%{http_code}' \
          "${forgejoApi}/users/${name}/tokens/${name}" \
          2>/dev/null || echo "000"
      )

      case "$delete_status" in
        204|404)
          ;;
        *)
          echo "Failed deleting existing token ${name}: HTTP $delete_status"
          exit 1
          ;;
      esac

      response_file=$(mktemp)
      create_status=$(
        curl \
          --silent \
          --max-time 5 \
          --config "$auth_file" \
          --header 'Content-Type: application/json' \
          --request POST \
          --output "$response_file" \
          --write-out '%{http_code}' \
          --data '{
            "name": "${name}",
            "scopes": ${scopesJson}
          }' \
          "${forgejoApi}/users/${name}/tokens" \
          2>/dev/null || echo "000"
      )

      if [ "$create_status" != 201 ]; then
        echo "Failed creating token ${name}: HTTP $create_status"
        cat "$response_file" || true
        exit 1
      fi

      token=$(jq -r '.sha1 // empty' "$response_file" 2>/dev/null || true)

      if [ -z "$token" ]; then
        echo "Token ${name} was created but no token value was returned"
        exit 1
      fi

      tmp_token_file=$(mktemp "$token_dir/.forgejo-${name}-token.XXXXXX")
      umask 0077
      printf '%s\n' "$token" > "$tmp_token_file"
      chmod 640 "$tmp_token_file"
      mv -f "$tmp_token_file" "$token_dest"
      tmp_token_file=""

      echo "Created API token for ${name}"
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

              scopes = mkOption {
                type = types.listOf types.str;
                default = [ "write:repository" ];
                defaultText = literalExpression ''[ "write:repository" ]'';
                description = "Forgejo token scopes. Must be a non-empty list; default grants the least privilege needed for hermes-agent repository writes.";
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
    assertions = lib.mapAttrsToList (name: token: {
      assertion = token.scopes != [ ];
      message = "myconfig.forgejo.client.tokens.${name}.scopes must not be empty.";
    }) cfg.tokens;

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
          Restart = "on-failure";
          RestartSec = "30s";
        };

        script = tokenScript name token;
      }
    ) cfg.tokens;
  };
}
