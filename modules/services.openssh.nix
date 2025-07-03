# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  lib,
  config,
  ...
}:

{
  config = {
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        X11Forwarding = config.services.xserver.enable;
      };
    };
    systemd.services.sshd.wantedBy = lib.mkOverride 40 [ "multi-user.target" ];
  };
}
