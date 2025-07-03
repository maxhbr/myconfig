# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  pkgs,
  config,
  ...
}:
{
  # config for libinput
  config = lib.mkIf config.services.xserver.libinput.enable {
    # services.xserver.synaptics.enable = lib.mkForce false;
    services.libinput.touchpad = {
      disableWhileTyping = true;
      tappingDragLock = false;
      naturalScrolling = true;
    };
  };
}
