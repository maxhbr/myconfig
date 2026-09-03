# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Automatic provisioning of Forgejo users on the host that runs the Forgejo
# service (see nixpkgs `services.forgejo`).
#
# For every user in `myconfig.forgejo.server.users` this module:
# - declares the matching `myconfig.secrets.<passwordSecret>` entry (owned by
#   the `forgejo` user; the secret itself must be provisioned in the `priv/`
#   repository),
# - creates the user via `forgejo admin user create` in the `preStart` of the
#   `forgejo.service` (idempotent: existing users are ignored),
# - orders `forgejo.service` after the agenix key services of the passwords,
# - for users with `gitSshKeys` set, pushes the given NixOS user's SSH
#   authorized keys to the Forgejo user (once, at boot) via the Forgejo API.
{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.myconfig.forgejo.server;

  # Ordered list of { name = Forgejo user name; user = user config; } pairs.
  # Attrset order is not preserved through module evaluation, so the creation
  # order is defined explicitly: admin users first (so the instance always has
  # an admin before any regular user), then alphabetical by user name.
  usersInOrder =
    lib.sort
      (
        a: b:
        [
          (if a.user.admin then 0 else 1)
          a.name
        ] < [
          (if b.user.admin then 0 else 1)
          b.name
        ]
      )
      (
        lib.mapAttrsToList (name: user: {
          inherit name;
          user = user;
        }) cfg.users
      );

  forgejoCfg = config.services.forgejo;
  forgejoPort = forgejoCfg.settings.server.HTTP_PORT;
  forgejoApi = "http://127.0.0.1:${toString forgejoPort}/api/v1";
  adminCmd = "${lib.getExe forgejoCfg.package} admin user";

  # One `forgejo admin user create` invocation per user; user creation order
  # is given by `usersInOrder` (admin users first, then alphabetical).
  createUserCommand =
    name: user:
    let
      secret = config.myconfig.secrets.${user.passwordSecret};
      lines = [
        "${adminCmd} create \\"
      ]
      ++ lib.optionals user.admin [ "--admin \\" ]
      ++ [
        "--email \"${user.email}\" \\"
        "--username ${name} \\"
        "--password \"\$(tr -d '\\n' < ${secret.dest})\" \\"
        "|| true"
      ];
      first = builtins.head lines;
      rest = builtins.tail lines;
    in
    "${first}\n" + lib.concatStringsSep "\n" (map (l: "  ${l}") rest);

  # One idempotent "add SSH key" command per (Forgejo user, key) pair; uses
  # the Forgejo user's own password for authentication.
  addKeyCommand =
    name: user: key:
    let
      secret = config.myconfig.secrets.${user.passwordSecret};
      fromUser = user.gitSshKeys;
      title = "nixos-${fromUser}-${builtins.substring 0 12 (builtins.hashString "sha256" key)}";
    in
    lib.concatStringsSep "\n" [
      "key_json=$(jq -cn \\"
      "  --arg title ${lib.escapeShellArg title} \\"
      "  --arg key ${lib.escapeShellArg key} \\"
      "  '{title: $title, key: $key, read_only: false}')"
      ""
      "user_password=$(tr -d '\\n' < ${secret.dest})"
      "auth_file=$(mktemp)"
      "response_file=$(mktemp)"
      "trap 'rm -f \"$auth_file\" \"$response_file\"' EXIT"
      "chmod 600 \"$auth_file\""
      "escaped_user_name=$(printf '%s' ${lib.escapeShellArg name} | sed -e 's/\\\\/\\\\\\\\/g' -e 's/\"/\\\\\"/g')"
      "escaped_user_password=$(printf '%s' \"$user_password\" | sed -e 's/\\\\/\\\\\\\\/g' -e 's/\"/\\\\\"/g')"
      "printf 'user = \"%s:%s\"\\n' \"$escaped_user_name\" \"$escaped_user_password\" > \"$auth_file\""
      ""
      "status=$("
      "  curl \\"
      "    --silent \\"
      "    --show-error \\"
      "    --max-time 10 \\"
      "    --output \"$response_file\" \\"
      "    --write-out '%{http_code}' \\"
      "    --config \"$auth_file\" \\"
      "    --header 'Content-Type: application/json' \\"
      "    --request POST \\"
      "    --data \"$key_json\" \\"
      "    \"${forgejoApi}/user/keys\""
      ")"
      ""
      "case \"$status\" in"
      "  201)"
      "    echo \"Added Forgejo SSH key ${title}\""
      "    ;;"
      "  422)"
      "    echo \"Forgejo SSH key ${title} already exists or is not accepted; ignoring\""
      "    ;;"
      "  *)"
      "    echo \"Unexpected status while adding Forgejo SSH key ${title}: $status\""
      "    cat \"$response_file\" || true"
      "    exit 1"
      "    ;;"
      "esac"
      ""
      "rm -f \"$auth_file\" \"$response_file\""
      "trap - EXIT"
    ];

  # Flattened list of { user = Forgejo user name; userCfg = ...; key = ...; }
  # for all users with `gitSshKeys` set, in creation order (`usersInOrder`).
  keyEntries = lib.concatMap (
    pair:
    if pair.user.gitSshKeys == null then
      [ ]
    else
      let
        keys = config.users.users.${pair.user.gitSshKeys}.openssh.authorizedKeys.keys;
      in
      map (key: {
        user = pair.name;
        userCfg = pair.user;
        key = key;
      }) keys
  ) usersInOrder;

  keyCommands = lib.concatMapStringsSep "\n" (
    e: addKeyCommand e.user e.userCfg e.key + "\n"
  ) keyEntries;

  keyUserPasswords = map (p: p.user.passwordSecret) (
    lib.filter (p: p.user.gitSshKeys != null) usersInOrder
  );
in
{
  options.myconfig.forgejo.server = with lib; {
    enable = mkEnableOption "myconfig.forgejo.server";

    users = mkOption {
      type = types.attrsOf (
        types.submodule (
          { name, ... }: {
            options = {
              email = mkOption {
                type = types.str;
                default = "${name}@localhost";
                defaultText = literalExpression ''"${name}@localhost"'';
                description = "Email address of the Forgejo user.";
              };

              admin = mkOption {
                type = types.bool;
                default = false;
                description = "Whether the Forgejo user gets the admin role.";
              };

              passwordSecret = mkOption {
                type = types.str;
                default = "forgejo-${name}-password";
                defaultText = literalExpression ''"forgejo-${name}-password"'';
                description = "Name of the `myconfig.secrets` entry holding the user's password. The secret is declared automatically (dest `/run/<passwordSecret>`, owned by the `forgejo` user) and must be provisioned in the `priv/` repository.";
              };

              gitSshKeys = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "Name of the NixOS user whose `openssh.authorizedKeys.keys` are pushed to this Forgejo user at boot (null = no SSH key provisioning).";
              };
            };
          }
        )
      );
      default = { };
      description = "Forgejo users to create automatically on this host.";
    };
  };

  config = lib.mkIf cfg.enable {
    myconfig.secrets = lib.listToAttrs (
      map (
        p:
        lib.nameValuePair p.user.passwordSecret {
          dest = "/run/${p.user.passwordSecret}";
          owner = "forgejo";
          group = "forgejo";
        }
      ) usersInOrder
    );

    systemd.services.forgejo = {
      serviceConfig.After = map (p: "${p.user.passwordSecret}-key.service") usersInOrder;

      preStart =
        lib.concatStringsSep "\n\n" (map (p: createUserCommand p.name p.user) usersInOrder) + "\n";
    };

    # Pushes SSH keys for every user with `gitSshKeys` set; only created when
    # at least one such user exists.
    systemd.services.forgejo-add-ssh-keys = lib.mkIf (keyEntries != [ ]) {
      description = "Add SSH keys from NixOS users to Forgejo users";
      wantedBy = [ "multi-user.target" ];
      after = [ "forgejo.service" ] ++ map (s: "${s}-key.service") keyUserPasswords;
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

        ${keyCommands}
      '';
    };
  };
}
