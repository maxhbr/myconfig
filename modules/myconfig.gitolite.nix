# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Reusable gitolite setup: enables `services.gitolite`, uses the main
# user's first RSA key as the admin key (the same selection the thing
# host made in its host-local `services.gitolite.nix`), and can scope
# gitolite access to the WireGuard mesh.
{
  config,
  lib,
  myconfig,
  ...
}:

let
  inherit (myconfig.metadatalib) get;

  cfg = config.myconfig.gitolite;

  # The wg subnet (`metadata.networks.<wgInterface>.allowedIPs`), or []
  # when this host / interface is not in the mesh.
  wgSubnets = wgInterface: (get.networks."${wgInterface}" or { }).allowedIPs or [ ];
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

    restrictToWg0 = mkEnableOption "restricting gitolite access to the wg0 subnet" // {
      description = ''
        gitolite shares the host sshd (port 22) with regular admin SSH,
        so the firewall cannot scope it. Instead, an sshd Match block
        denies the `gitolite` user for clients outside the wg0 subnet
        (`metadata.networks.wg0.allowedIPs`); admin logins over other
        interfaces stay possible.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.gitolite = {
      enable = true;
      inherit (cfg) adminPubkey;
    };

    services.openssh.extraConfig = lib.mkIf cfg.restrictToWg0 (
      lib.concatStringsSep "\n" (
        [ "Match User gitolite Address *,!${lib.concatStringsSep ",!" (wgSubnets "wg0")}" ]
        ++ [ "  DenyUsers gitolite" ]
      )
    );
  };
}
