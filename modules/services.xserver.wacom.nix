# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  config = lib.mkIf (config.services.xserver.wacom.enable) {
    environment.systemPackages = with pkgs; [ xf86_input_wacom ];
    hardware.opentabletdriver.enable = true;
  };
}
