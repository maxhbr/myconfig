# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ lib, pkgs, config, ... }: { # config for synaptics (unused?)
  config = lib.mkIf config.services.xserver.synaptics.enable {
    # services.xserver.libinput.enable = lib.mkForce true;
    services.xserver.synaptics = {
      twoFingerScroll = true;
      vertEdgeScroll = false;
    };
  };
}
