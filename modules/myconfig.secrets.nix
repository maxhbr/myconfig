{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:

with lib;

let
  cfg = config.myconfig.secrets;

  secret = types.submodule {
    options = {
      source = mkOption {
        default = null;
        type = types.nullOr types.path;
        description = "local secret path";
      };

      dest = mkOption {
        type = types.str;
        description = "where to write the decrypted secret to";
      };

      owner = mkOption {
        default = "root";
        type = types.str;
        description = "who should own the secret";
      };

      group = mkOption {
        default = "root";
        type = types.str;
        description = "what group should own the secret";
      };

      permissions = mkOption {
        default = "0400";
        type = types.str;
        description = "Permissions expressed as octal.";
      };

      wantedBy = mkOption {
        default = [ ];
        type = types.listOf types.str;
        description = "Other targets that depend on this secret.";
      };

      unsafePubkeyOverwrite = mkOption {
        default = null;
        type = types.nullOr types.path;
        description = "overwrite the used pubkey.";
      };

      unsafePrivkeyOverwrite = mkOption {
        default = null;
        type = types.nullOr types.path;
        description = "overwrite the used privkey.";
      };

      symlink = mkOption {
        default = true;
        type = types.bool;
        description = ''
          Whether to symlink the secret to its destination (agenix default)
          or decrypt it directly to `dest`.

          Set to `false` when the `dest` path is bind-mounted into a NixOS
          container, since a symlink target under `/run/agenix` would not
          be visible inside the container's mount namespace.
        '';
      };
    };
  };

  mkEncryptedAgeFile =
    name:
    { source, unsafePubkeyOverwrite, ... }:
    pkgs.runCommand "${name}.age"
      {
        nativeBuildInputs = [ pkgs.age ];
      }
      (
        let
          pubkeyArg =
            if unsafePubkeyOverwrite == null then
              "-r '${
                myconfig.metadatalib.get.hosts."${config.networking.hostName
                }".pubkeys."/etc/ssh/ssh_host_rsa_key.pub"
              }'"
            else
              "-R '${unsafePubkeyOverwrite}'";
        in
        ''
          age -a ${pubkeyArg} -o "$out" '${source}'
        ''
      );

  validSecrets = filterAttrs (_name: info: info.source != null) cfg;

  missingSource = filterAttrs (_name: info: info.source == null) cfg;

  secretsWithPrivkeyOverride = filterAttrs (
    _name: info: info.unsafePrivkeyOverwrite != null
  ) validSecrets;

in
{
  options.myconfig.secrets = mkOption {
    type = types.attrsOf secret;
    description = "secret configuration";
    default = { };
  };

  config = mkMerge [
    {
      warnings =
        optional (
          missingSource != { }
        ) "myconfig.secrets: source is missing for: ${concatStringsSep ", " (attrNames missingSource)}"
        ++
          optional (secretsWithPrivkeyOverride != { })
            "myconfig.secrets: unsafePrivkeyOverwrite cannot be represented per-secret by agenix; using all override keys globally via age.identityPaths.";

      age.secrets = mapAttrs (name: info: {
        file = mkEncryptedAgeFile name info;
        path = info.dest;
        owner = info.owner;
        group = info.group;
        mode = info.permissions;
        inherit (info) symlink;
      }) validSecrets;
    }

    /*
      Your old module allowed per-secret private-key overrides.

      Agenix has a global `age.identityPaths`, not a per-secret private key
      option. This preserves behavior approximately by adding all overridden
      private keys to the global identity list.
    */
    (mkIf (secretsWithPrivkeyOverride != { }) {
      age.identityPaths = mapAttrsToList (
        _name: info: toString info.unsafePrivkeyOverwrite
      ) secretsWithPrivkeyOverride;
    })

    /*
      Your old `wantedBy` made custom systemd units run before specific targets.
      Agenix normally handles secrets during activation. If you had services
      relying on old `wantedBy` ordering, prefer changing those services to
      depend on agenix's activation result or simply reference the final path.

      In most cases, no explicit ordering is needed.
    */
  ];
}
