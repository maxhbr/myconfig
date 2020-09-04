# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  imports = [ ../desktop.X.common ];

  config = {
    # home-manager.users.mhuber = {
    #   xsession.windowManager.awesome.enable = true;
    # };
    services.xserver = { windowManager.awesome = { enable = true; }; };
  };
}
