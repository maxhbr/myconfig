# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let user = config.myconfig.user;
in {
  imports = [
    ../modules
    # configuration
  ];

  config = {
    myconfig.headless.enable = true;
  };
}
