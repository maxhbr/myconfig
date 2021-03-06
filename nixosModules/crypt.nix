{ pkgs, config, lib, ... }:
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
    };
  };

  metadata = lib.importTOML ../../ops/metadata/hosts.toml;

  mkSecretOnDisk = name:
    { source, ... }:
    pkgs.stdenv.mkDerivation {
      name = "${name}-secret";
      phases = "installPhase";
      buildInputs = [ pkgs.age ];
      installPhase =
        let key = metadata.hosts."${config.networking.hostName}".ssh_pubkey;
        in ''
          age -a -r '${key}' -o "$out" '${source}'
        '';
    };

  mkService = name:
    { source, dest, owner, group, permissions, ... }: {
      description = "decrypt secret for ${name}";
      wantedBy = [ "multi-user.target" ];

      serviceConfig.Type = "oneshot";

      script = with pkgs; ''
        rm -rf ${dest}
        "${age}"/bin/age -d -i /etc/ssh/ssh_host_ed25519_key -o '${dest}' '${
          mkSecretOnDisk name { inherit source; }
        }'
        chown '${owner}':'${group}' '${dest}'
        chmod '${permissions}' '${dest}'
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
