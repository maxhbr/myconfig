{ pkgs, config, lib, myconfig, ... }:
# copied from https://github.com/Xe/nixos-configs/blob/master/common/crypto/default.nix : MIT
# see also: https://christine.website/blog/nixos-encrypted-secrets-2021-01-20
# License: MIT
# Copyright (c) 2020 Christine Dodrill <me@christine.website>

with lib;

let
  cfg = config.myconfig.secrets;

  secret = types.submodule {
    options = {
      source = mkOption {
        type = types.path;
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
    };
  };

  mkSecretOnDisk = name:
    { source, unsafePubkeyOverwrite, ... }:
    pkgs.stdenv.mkDerivation {
      name = "${name}-secret";
      phases = "installPhase";
      installPhase = let
        pubkeyArgs = if unsafePubkeyOverwrite == null then
          "-r '${
            myconfig.metadatalib.get.hosts."${config.networking.hostName}".pubkeys."/etc/ssh/ssh_host_rsa_key.pub"
          }'"
        else
          "-R '${unsafePubkeyOverwrite}'";
      in ''
        "${pkgs.age}"/bin/age -a ${pubkeyArgs} -o "$out" '${source}'
      '';
    };

  mkService = name:
    { source, dest, owner, group, permissions, wantedBy, unsafePubkeyOverwrite
    , unsafePrivkeyOverwrite, ... }: {
      description = "decrypt secret for ${name}";
      wantedBy = [ "multi-user.target" ] ++ wantedBy;
      before = wantedBy;

      serviceConfig.Type = "oneshot";

      script = let
        privkey = if unsafePrivkeyOverwrite == null then
          "/etc/ssh/ssh_host_rsa_key"
        else
          unsafePrivkeyOverwrite;
      in with pkgs; ''
        dir="$(dirname '${dest}')"
        if [[ ! -d "$dir" ]]; then
          mkdir -p "$dir"
          chown '${owner}':'${group}' "$dir"
        fi

        rm -rf '${dest}'
        "${age}"/bin/age -d \
            -i '${privkey}' -o '${dest}' \
            '${mkSecretOnDisk name { inherit source unsafePubkeyOverwrite; }}'

        chown '${owner}':'${group}' '${dest}'
        chmod '${permissions}' '${dest}'

        # # test for readability, fails if a parent folder is not readable
        # sudo -H -u '${owner}' -g '${group}' bash -c "test -r '${dest}'"
      '';
    };
in {
  options.myconfig.secrets = mkOption {
    type = types.attrsOf secret;
    description = "secret configuration";
    default = { };
  };

  config.systemd.services = let
    units = mapAttrs' (name: info: {
      name = "${name}-key";
      value = (mkService name info);
    }) cfg;
  in units;
}
