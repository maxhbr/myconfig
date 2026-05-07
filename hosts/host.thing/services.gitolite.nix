# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:

{
  config = {
    services.gitolite = {
      enable = true;
      adminPubkey = (
        builtins.head (
          builtins.filter (k: lib.hasPrefix "ssh-rsa" k)
            config.users.extraUsers."${myconfig.user}".openssh.authorizedKeys.keys
        )
      );
    };
  };
}
