# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Reusable gitolite setup: enables `services.gitolite` and uses the
# main user's first RSA key as the admin key (the same selection the
# thing host made in its host-local `services.gitolite.nix`).
{
  config,
  lib,
  myconfig,
  ...
}:

let
  cfg = config.myconfig.gitolite;
in
{
  options.myconfig.gitolite = with lib; {
    enable = mkEnableOption "myconfig.gitolite";

    adminPubkey = mkOption {
      type = types.nullOr types.str;
      default = builtins.head (
        builtins.filter (k: lib.hasPrefix "ssh-rsa" k)
          config.users.extraUsers."${myconfig.user}".openssh.authorizedKeys.keys
      );
      defaultText = literalExpression "first ssh-rsa key of \${myconfig.user}";
      description = ''
        Public key that becomes the gitolite admin. Defaults to the
        first `ssh-rsa` key of the main user's authorized keys, so the
        stock deploy yields a working admin clone out of the box.
        Set explicitly to use a different key (e.g. an ed25519 one).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.gitolite = {
      enable = true;
      inherit (cfg) adminPubkey;
    };
  };
}
